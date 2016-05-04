#Day2 Misc Problems
simple = list(1,2,3)
depth_two = list(1, list(2,3))
nest_test = list(list(1,2,3),list(4,5,list(6)))

nesting_depth = function(nested_list) {
  count = 0
  while(typeof(nested_list) == "list") {
    nested_list = unlist(nested_list, recursive = FALSE)
    count = count + 1
  }
  return(count)
}

unique_dominoes = function(n) {
  dominoes = list()
  for (i in 0:n) {
    for (j in i:n) {
      dominoes[[length(dominoes)+1]] = c(i,j)
    }
  }
  return(dominoes)
}

unique_dominoes(2)

valid = list(list(1, 2), list(2, 3), list(3, 1))

is_circle = function(L) {
  for (i in 1:(length(L)-1)){
    first = unlist(L[i])[2]
    second = unlist(L[i+1])[1]
    if (first != second) {
      return(FALSE)
    }
  }
  if (unlist(L[1])[1] != unlist(L[length(L)])[2]) {
    return(FALSE)
  }
  return(TRUE)
}

is_circle(valid)

#Data Frames

df = data.frame(1:4,1:4,2:5)
  
count_letters = function(df){
  letter_counts = c()
  #names = as.character(names(df))
  names = names(df)
  for (letter in letters) {
    count = 0
    for (name in names) {
      matches = unlist(gregexpr(letter, name))  
      if (matches[1] != -1){
        count = count + length(matches)
      }
    }
    letter_counts = c(letter_counts,count)
  }  
  names(letter_counts) = letters
  return(letter_counts)
}

# grepl, increment if true, then replace the letter, keep going 

fix_spaces = function(df) {
  names = names(df)
  names(df) = gsub(' ', '.', names)
  names = names(df)
  toassign = c()
  for (name in names) {
    name = paste(name,'_mod',sep = '')
    toassign = c(toassign, name)
  }
  names(df) = toassign
  return(df)
}

four_fewer = function(df) {
  names = names(df)
  new_names = c()
  for (name in names) {
    len = nchar(name)
    new_name = character(0)
    if (len > 4) {
       new_name = substr(name, 1, len - 4)
    }
    new_names = c(new_names, new_name)
  }
  #print(new_names)
  names(df) = new_names
  return(df)
}
head(four_fewer(df))

print_row_names = function(df) {
   paste(row.names(df), collapse = '_')
}
print_row_names(df)

num_data = data.frame(matrix(1:24, nrow=6))
num_data

lessthan3 = function(df) {
  curdf = df
  #if (sum(dim(curdf)) != 0) {
      # vector = getPerimeter
      # vector = c(vector, spiral(smallerdf))
  # west
  spiral=c()
  while (TRUE) {
    spiral = c(spiral,curdf[[1]])
    
    if (dim(curdf)[2] == 1) {
      return(c(spiral,curdf[[1]]))
    }
    curdf = data.frame(curdf[,2:length(curdf)])
    #south
    spiral = c(spiral,curdf[nrow(curdf),])
    
    if (dim(curdf)[1] == 1) {
      return(c(spiral,curdf[1,1]))
    }
    curdf = data.frame(curdf[1:nrow(curdf)-1, ])
    #east
    spiral = c(spiral,curdf[nrow(curdf):1,length(curdf)])
    
    if (dim(curdf)[2] == 1) {
      return(c(spiral,curdf[,1:(length(curdf)-1)]))
    }
    curdf = data.frame(curdf[,1:(length(curdf)-1)])
    
    spiral = c(spiral,curdf[1,length(curdf):1])
    
    if (dim(curdf)[1] == 1) {
      return(c(spiral,curdf[1,length(curdf):1]))
    }
    curdf = data.frame(curdf[2:nrow(curdf), ])
    #return(spiral)
  }
}
unlist(lessthan3(num_data))
length(unlist(lessthan3(num_data)))

spiral = function(df) {
  spiral2(df, 1, 1, nrow(df), length(df)) 
}

spiral2 = function(df, startRow, startCol, rowLen, colLen) {
  # Base case checking
  ### check if dimensions are 3x3 or greater
  #if (startRow > rowLen ||)
  if (sum(dim(df)) == 0) {
    return(c()) 
  }
  colLen = length(df)
  rowLen = nrow(df)
  
  # Compute outer spiral
  spiralList = c()
  # left
  spiralList = c(spiralList, df[[startCol]])
  # bottom
  selectedRow = df[rowLen,]
  selectedRow = selectedRow[(startCol+1):colLen]
  spiralList = c(spiralList, selectedRow) 
  # right
  selectedCol = df[(rowLen-1):startRow,colLen]
  spiralList = c(spiralList, selectedCol)
  # top
  selectedRow = df[startRow,(colLen-1):(startRow+1)]
  spiralList = c(spiralList, selectedRow)
    
  # Recursive step
  spiral2(df, startRow + 1, startCol + 1, rowLen - 1, colLen - 1)
  
  return(unlist(spiralList))
}

num_data = data.frame(matrix(1:9, nrow=3))
spiral(num_data)
