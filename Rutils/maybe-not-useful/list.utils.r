#==========================================================================================#
#==========================================================================================#
#     This function assigns a new element in the list with the given name.                 #
#------------------------------------------------------------------------------------------#
list.assign <<- function(l,x,value){
   l[[x]] = value
   return(l)
}#end list.assign
#==========================================================================================#
#==========================================================================================#



#------------------------------------------------------------------------------------------#
#    Join two lists.                                                                       #
#------------------------------------------------------------------------------------------#
list.join <<- function(x,y){
   ans        = unlist(c(x,y))
   names(ans) = NULL
   return     = ans
}#end ljoin
#------------------------------------------------------------------------------------------#