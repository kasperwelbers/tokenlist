interpretCodebook <- function(cb, code_columns){
  code = cb[,code_columns]
  code[is.na(code)] = ''

  cb = data.frame(code_id = cb$code_id,
                  code = as.character(apply(code, 1, paste, collapse='')),
                  level = apply(code, 1, function(x) min(which(!x == ''))), # first column with text
                  indicator = as.character(cb$indicator),
                  condition = as.character(cb$condition))

  cb$parent_id = NA
  parents = NA
  for(i in 1:nrow(cb)){
    j = cb$level[i]
    parents[j] = cb$code_id[i]
    cb$parent_id[i] = ifelse(j > 1, parents[j-1], NA)
  }
  cb$parent = cb$code[match(cb$parent_id, cb$code_id)]
  cb
}

listlevel <- function(x) {
  l = as.list(rep('', length(x)))
  names(l) = x
  l
}

listTree <- function(pc, parent='root'){
  codes = na.omit(pc$code[pc$parent == parent])
  tree = list()
  tree[[parent]] = listlevel(codes)
  for(code in codes){
    if(code %in% pc$parent) tree[[parent]][code] = listTree(pc, code)
  }
  tree
}

getCodeTree <- function(cb){
  pc =  data.frame(parent = sprintf('%s: %s', cb$parent_id, cb$parent),
                   code = sprintf('%s: %s', cb$code_id, cb$code),
                   stringsAsFactors = F)

  pc$parent[!pc$parent %in% pc$code] = 'root'
  listTree(pc)[['root']]
}

getCodeLevel <- function(cb){
  cb$level = NA
  cb$level[is.na(cb$parent)] = 1
  while(TRUE){
    level = max(cb$level, na.rm = T)
    levelparents = na.omit(cb$code[cb$level == level])
    nextlevel = cb$parent %in% levelparents
    if(sum(nextlevel) == 0) break
    cb$level[nextlevel] = level+1
  }
}


indentLevel <- function(code, level){
  indent = rep('__', level-1)
  paste(indent, code, sep='')
}

nwhitespaces <- function(n) paste(rep('&nbsp;', n), collapse='')

codebookListItem <- function(code, code_id, level){
  sprintf('<div class="codebookitem">
          %s
          <label>
          <input type="radio" name="code_id" value="%s">
          %s
          </label>
          </div>
          ', nwhitespaces(level*10), code_id, code)
}

codebookList <- function(cb){
  if(is.null(cb)) return(NULL)
  codes = mapply(codebookListItem, code=cb$code, code_id=cb$code_id, level=cb$level - min(cb$level))
  codes = paste(codes, collapse='\n')
  HTML(sprintf('
               <div id="code_id", class = "form-group shiny-input-radiogroup shiny-input-container shiny-bound-input">
               <div class="shiny-options-group">
               %s
               </div>
               </div>
               ', codes))
}


spanAbrev <- function(x, charlength=15){
  x = as.character(x)
  ifelse(str_length(x) > charlength, sprintf('<span title="%s">%s...</span>', x, str_sub(x, 1, charlength)), x)
}

codebookDF <- function(cb){
  if(is.null(cb)) return(NULL)
  d = NULL

  #linkform = '<label for="%s"><input id=%s type="radio" name="code_id" value="%s">%s</label>'

  clevels =  min(cb$level):max(cb$level)
  for(level in clevels){
    #d = cbind(d, ifelse(cb$level == level, sprintf(linkform, cb$code_id, cb$code_id, cb$code_id, spanAbrev(cb$code)), ''))
    d = cbind(d, ifelse(cb$level == level, spanAbrev(cb$code), ''))
  }
  d = as.data.frame(d)
  colnames(d) = clevels
  rownames(d) = cb$code_id
  d
}

queryhelp <- function(){
  HTML(
    '<h5>A query consists out of an indicator, and optionally a condition.</h5>
    The indicator... <ul>
    <li>is the actual text that has to be found in the token</li>
    <li>can contain multiple words with OR statement (and empty spaces are also considered OR statements)</li>
    <li>CANNOT contain AND or NOT statements (this is what the condition is for)</li>
    <li>accepts the ? wildcard, which means that any single character can be used in this place</li>
    <li>accepts the * wildcard, which means that any number of characters can be used in this place</li>
    </ul>

    The condition... <ul>
    <li>has to be TRUE for the indicator to be accepted. Thus, if a condition is given, the query can be interpreted as: indicator AND condition</li>
    <li>can contain complex boolean statements, using AND, OR and NOT statements, and using parentheses</li>
    <li>accepts the ? and * wildcards</li>
    <li>can be specified for a maximum word distance of the indicator. This is done with the ~ symbol, where "example~20" means that the word "example" is looked up within 20 words of the indicator. </li>
    </ul>
    '
  )
}

getHits <- function(tokens, session, input, indicator, condition){
  if(input$indicator == '') return(NULL)

  # do not allow linebreaks in query fields
  updateTextInput(session, 'indicator', value=gsub('\n', ' ', indicator, fixed=T))
  updateTextInput(session, 'condition', value=gsub('\n', ' ', condition, fixed=T))

  #default_window = ifelse(input$use_window == 'window', input$nwords, NA)
  default_window = NA
  searchQuery(tokens, indicator = indicator, condition=condition, default.window = default_window, condition_once = input$condition_once)
}

prepareResults <- function(tokens, input, hits){
  hitcount = reportHitcount(tokens, hits)

  if(nrow(hits) == 0) {
    return(list(hitcount=hitcount))
  } else{
    tokenfreq = reportTokenFreq(hits)
    tokenfreq = tokenfreq[order(-tokenfreq$hits),]
    samplesize = ifelse(input$sample_n > nrow(hits), nrow(hits), input$sample_n)
    hits = hits[sample(1:nrow(hits), samplesize),]
    kwic = getKwic(tokens, hits, input$kwic_words)
    return(list(hitcount=hitcount, tokens=tokens, hits=hits, tokenfreq=tokenfreq, kwic=kwic))
  }
}

hitsX_not_hitsY <- function(x, y) {
  x_not_y = !paste(x$doc_id, x$position, sep='---') %in% paste(y$doc_id, y$position, sep='---')
  x[x_not_y,]
}


