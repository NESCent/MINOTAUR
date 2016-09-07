
##################
## MESSAGE MENU ##  ------------------------------------------------------------------------------------
##################

#### The message menu is set up to display error messages to the user if something isn't working properly. The way to do this is as follows:
# 1) All possible errors used by the program are defined a priori in the errorMenu$message list. The name of the message (for example 'too_many_chroms') is considered the error_id, and both the text and the icon of this message can be defined.
# 2) In other parts of the code, errors can be turned on and off using the functions errorOn(error_id) and errorOff(error_id)
# 3) When the program runs it looks through all items in errorMenu$message, and those that are turned on are added to the list and rendered
# 4) If there is at least one error the style of the menu will change (the icon will start spinning!) to remind the user that something needs sorting

# set up a list of possible errors
errorMenu <- shiny::reactiveValues()
errorMenu$message <- list('too_many_chroms'=list(text='Long messages like this dont seem to print in their entirity. Ive posted a question on StackOverflow to try and get this working!',
                                                 icon='search',
                                                 on=FALSE),
                          'too_many_pies'=list(text='who ate all the pies?',
                                               icon='bars',
                                               on=FALSE)
                          )


# functions for quickly turning errors on and off
errorOn <- function(error_id) {
  errorMenu$message[[error_id]]$on <- TRUE
}
errorOff <- function(error_id) {
  errorMenu$message[[error_id]]$on <- FALSE
}

# create menu containing error messages
output$messageMenu <- shinydashboard::renderMenu({

  # make list of shiny error elements
  msgs <- list()
  for (i in 1:length(errorMenu$message)) {
    if (errorMenu$message[[i]]$on) {
      msgs[[length(msgs)+1]] <- notificationItem(text=errorMenu$message[[i]]$text,icon('search'),status='danger')
    }
  }

  # make notification menu (format depends on number of error messages)
  if (length(msgs)==0) {
      #shinydashboard::dropdownMenu(type='notifications',.list=msgs, icon=HTML('<i class="fa fa-times-circle fa-2x"></i>'), badgeStatus='success')
      shinydashboard::dropdownMenu(type='notifications',.list=msgs, icon=HTML('<i class="fa fa-smile-o"></i>'))
  } else {
      shinydashboard::dropdownMenu(type='notifications',.list=msgs, icon=HTML('<i class="fa fa-times-circle fa-spin fa-2x"></i>'), badgeStatus='danger')
      #shinydashboard::dropdownMenu(type='notifications',.list=msgs, icon=HTML('<i class="fa fa-warning fa-spin"></i>'), badgeStatus='danger')
  }
})
