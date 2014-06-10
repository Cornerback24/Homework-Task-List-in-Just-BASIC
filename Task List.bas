'This is the homework task list program
'Made by me! (Nicholas Masching)
'New in this update:
'   alt+z undoes any deletions from the current session
'   back up file created when program run

'In tasks file, tasks are:
    'mm/dd/yyyy
    'TurnIn
    'Class
    'Description
    '
    '(Each on its own line, no extra line between tasks)
    'TurnIn is Yes or No or Exam or Event
    'Number on top is number of tasks
    'null is blank

'arrays
'   daysLeft$()
'   turnIn()
'   dueDate()
'   class()
'   description()

    dim daysLeft(0) 'This array has numbers, they're easier to sort (currently seems to be used only as temporary variable in calculation days left
                    'mabye change this?  Sorting is done by walk down list when add a new task until place found by date, and edit done by deleting and
                    'then re-creating task.)
    dim daysLeft$(0)
    dim turnIn$(0)
    dim dueDate$(0)
    dim class$(0)
    dim description$(0)
    dim deleteStack$(0) 'holds that tasks that have been deleted in case of alt+z restore
                        'holds task details in linear order: dueDate$, turnIn$, class$, description$
    global numTasks 'the current number of tasks
    global activeTask 'the tasks currently being viewed
    global numDeletedTasks 'the number of tasks in the deleted tasks queue queue


    nomainwin

    WindowWidth = 1047
    WindowHeight = 555

    statictext #main.statictext1, "Days Left", 40, 50, 64, 20
    statictext #main.statictext2, "Turn In?", 110, 50, 50, 20
    listbox #main.daysLeft, daysLeft$(, lb1, 40, 75, 55, 350
    listbox #main.turnIn, turnIn$(, lb2, 110, 75, 75, 350
    statictext #main.statictext5, "Class", 195, 50, 40, 20
    listbox #main.class, class$(, lb3, 195, 75, 200, 350
    statictext #main.statictext7, "Due Date", 405, 50, 64, 20
    listbox #main.dueDate, dueDate$(, lb4, 405, 75, 200, 350
    statictext #main.statictext9, "Description", 615, 50, 88, 20
    listbox #main.description, description$(, lb5, 615, 75, 400, 350
    button #main.add, "&Add Task", add, UL, 40, 446, 100, 50
    button #main.delete, "&Finished (Delete)", delete, UL, 179, 446, 150, 50
    button #main.edit, "&Edit Task", edit, UL, 361, 446, 100, 50
    button #main.viewDescription, "&View Description", viewDescription, UL, 498, 446, 150, 50
    button #main.restoreTask, "&z", restoreTask, UL, -20, -20, 0, 0


    UpperLeftX = DisplayWidth/2 - 1032/2
    UpperLeftY = DisplayHeight/2 - 555/2 - DisplayHeight/16

    open "Task List" for window as #main

    print #main, "font ms_sans_serif 0 16"
    print #main, "trapclose trapclose"
    'each lists activated on single-click, to highlight others
    print #main.daysLeft, "singleclickselect"
    print #main.turnIn, "singleclickselect"
    print #main.class, "singleclickselect"
    print #main.description, "singleclickselect"
    print #main.dueDate, "singleclickselect"

    call load
    call createBackUp
    'calculate the number of days left for each task
    for i = 1 to numTasks
        call calcDaysLeft i
    next i
    'load the info into the listboxes
    call reload
    'set active task to the first item
    activeTask = 1
    call listBoxSelect

    if numTasks = 0 then call disable
    print #main.daysLeft, "setfocus"

    'Super important line!
    wait



    sub trapclose windowhandle$
        call save
        close #main
        stop
    end sub

    'loads the tasks
    sub load
        open "tasks.txt" for input as #tasks
            input #tasks, numTasks
            redim daysLeft(numTasks)
            redim daysLeft$(numTasks)
            redim turnIn$(numTasks)
            redim class$(numTasks)
            redim description$(numTasks)
            redim dueDate$(numTasks)
            'load tasks into array
            for i = 1 to numTasks
                'This order is important!
                line input #tasks, dueDate$(i)
                line input #tasks, turnIn$(i)
                line input #tasks, class$(i)
                line input #tasks, description$(i)
            next i
        close #tasks
    end sub

    'reloads all listboxes
    sub reload
        print #main.daysLeft, "reload"
        print #main.turnIn, "reload"
        print #main.class, "reload"
        print #main.dueDate, "reload"
        print #main.description, "reload"
    end sub

    'calculates the number of days left for assignment i
    sub calcDaysLeft i
        daysLeft(i) = date$(dueDate$(i)) - date$("days")
        daysLeft$(i) = str$(daysLeft(i))
    end sub


    'selects activeTask in all listboxes
    sub listBoxSelect
        print #main.daysLeft, "selectindex ";activeTask
        print #main.turnIn, "selectindex ";activeTask
        print #main.class, "selectindex ";activeTask
        print #main.dueDate, "selectindex ";activeTask
        print #main.description, "selectindex ";activeTask
    end sub

    'These routines set the activeTask based on which listbox is clicked and which index is selected
    sub lb1 windowhandle$
        print #main.daysLeft, "selectionindex? i"
        activeTask = i
        call listBoxSelect
    end sub
    sub lb2 windowhandle$
        print #main.turnIn, "selectionindex? i"
        activeTask = i
        call listBoxSelect
    end sub
    sub lb3 windowhandle$
        print #main.class, "selectionindex? i"
        activeTask = i
        call listBoxSelect
    end sub
    sub lb4 windowhandle$
        print #main.dueDate, "selectionindex? i"
        activeTask = i
        call listBoxSelect
    end sub
    sub lb5 windowhandle$ 
        print #main.description, "selectionindex? i"
        activeTask = i
        call listBoxSelect
    end sub

    'shows a notice so that the entire description of a task can be viewed
    sub viewDescription windowhandle$
        print #main.description, "selectionindex? i"
        notice class$(i) + chr$(13) + description$(i)
        print #main.daysLeft, "setfocus"
    end sub

    function processDate$(dateString$)
        dateString$ = trim$(dateString$)

        'check for a string with no year
        if instr(dateString$,"/") then
            if  not( instr(dateString$,"/",instr(dateString$,"/")+1 )) then
            month$ = left$(dateString$,instr(dateString$,"/")-1)
            day$ = mid$(dateString$,instr(dateString$,"/")+1)

            'make sure montth is not invalid and day is at least a number
            if val(month$) > 0 and val(month$) < 13 and val(day$) > 0 then
                year$ = right$(date$("mm/dd/yyyy"),4)
                'make sure the date is not in the past
                if date$(month$ + "/" + day$ + "/" + year$) - date$("days") < 0 then
                    year$ = str$(val(year$) + 1)
                end if

                dateString$ = month$ + "/" + day$ + "/" + year$
            else
                dateString$ = ""
            end if

            end if
        end if




        if date$(dateString$) = 0 then dateString$ = date$(date$("days") + val(dateString$))
        if dateString$ = "" then dateString$ = date$(date$(date$()))

        processDate$ = dateString$
    end function

    'add a new task (windowhandle$ = "#main.add")
    'also used to edit a task (windowhandle$ = "edit"), set because called from the edit sub
    sub add windowhandle$ 

        WindowWidth = 464
        WindowHeight = 340

        statictext #add.statictext4, "Due Date (mm/dd/yyyy) or enter number of days until due: ", 25, 16, 360, 20
        textbox #add.date, 25, 41, 152, 25
        statictext #add.statictext8, "Class", 25, 90, 40, 20
        textbox #add.class, 25, 110, 240, 30
        textbox #add.description, 25, 180, 352, 20
        statictext #add.statictext13, "Description", 25, 160, 88, 20
        groupbox #add.turnInStatus, "Turn In?", 300, 56, 100, 110
        radiobutton #add.no, "No", [radiowait], [radiowait], 310, 71, 72, 20
        radiobutton #add.yes, "Yes", [radiowait], [radiowait], 310, 91, 72, 20
        radiobutton #add.exam, "Exam", [radiowait], [radiowait], 310, 111, 72, 20
        radiobutton #add.event, "Event", [radiowait], [radiowait], 310, 131, 72, 20
        button #add.default, "OK", [OK], UL, 25, 235, 100, 50
        button #add.button18, "Cancel", [closeAdd], UL, 150, 235, 100, 50
        button #add.t, "&t", [hidden], UL, -50, -50, 0, 0
        button #add.e, "&e", [hidden2], UL, -50, -50, 0, 0
        button #add.hidbugfix1, "&a", doNothing, UL, -50, -50, 0, 0
        button #add.hidbugfix2, "&f", doNothing, UL, -50, -50, 0, 0
        button #add.hidbugfix4, "&v", doNothing, UL, -50, -50, 0, 0
        open "Add Task" for dialog_nf_modal as #add
        print #add, "font ms_sans_serif 0 16"
        print #add, "trapclose [closeAdd]"
        print #add.yes, "set"
        print #add.date, "!setfocus"

        'load info if editing the task
        if windowhandle$ = "edit" then
            print #add.date, dueDate$(activeTask)
            print #add.class, class$(activeTask)
            print #add.description, description$(activeTask)
            if turnIn$(activeTask) = "No" then print #add.no, "set"
            if turnIn$(activeTask) = "Yes" then print #add.yes, "set"
            if turnIn$(activeTask) = "Exam" then print #add.exam, "set"
            if turnIn$(activeTask) = "Event" then print #add.event, "set"
        end if

        [add.inputLoop]
        wait

        'radiobuttons don't need special handling
        [radiowait]
        goto [add.inputLoop]


        'add task
        [OK]
        print #add.class, "!contents? class$"
        print #add.description, "!contents? description$"
        print #add.date, "!contents? date$"
        'set the date string if number of days entered
        date$ = processDate$(date$)
        'if date$(date$) = 0 then date$ = date$(date$("days") + val(date$))
        'if date$ = "" then date$ = date$(date$(date$()))
        'set turnIn
        turnIn$ = "No"
        print #add.yes, "value? result$"
        if result$ = "set" then turnIn$ = "Yes"
        print #add.exam, "value? result$"
        if result$ = "set" then turnIn$ = "Exam"
        print #add.event, "value? result$"
        if result$ = "set" then turnIn$ = "Event"

        'delete the task and then add it if editing (so that date changes a properly reordered)
        if windowhandle$ = "edit" then
            call deleteTask
            'replace empty strings with spaces to avoid problems
            if class$ = "" then class$ = " "
            if description$ = "" then description$ = " "
        end if

        call addTask date$, turnIn$, class$, description$

        call reload
        call listBoxSelect
        if windowhandle$ = "#main.add" then
            if numTasks = 1 then call enable
        end if

        [closeAdd]
        close #add
        print #main.daysLeft, "setfocus"

    end sub


    'adds a task to the arrays
    sub addTask date$, turnIn$, class$, description$
        'emtpy strings don't work well
        if class$ = "" then class$ = " "
        if description$ = "" then description$ = " "
        daysLeft = date$(date$) - date$("days")
        'find out where in the arrays the new task should be stored
        i = numTasks
        while daysLeft < val(daysLeft$(i)) and i > 0
            i = i -1
        wend
        placement = i + 1
        'shift pass date in arrays down
        'first create temp arrays
        call createTempArrays
        'add one to tasks
        numTasks = numTasks + 1
        redim class$(numTasks)
        redim description$(numTasks)
        redim turnIn$(numTasks)
        redim daysLeft$(numTasks)
        redim dueDate$(numTasks)
        'create new arrays
        for i = 1 to placement - 1
            class$(i) = tempClass$(i)
            description$(i) = tempDescription$(i)
            turnIn$(i) = tempTurnIn$(i)
            daysLeft$(i) = tempDaysLeft$(i)
            dueDate$(i) = tempDueDate$(i)
        next i
        class$(placement) = class$
        dueDate$(placement) = date$
        turnIn$(placement) = turnIn$
        description$(placement) = description$
        daysLeft$(placement) = str$(daysLeft)
        for i = placement + 1 to numTasks
            class$(i) = tempClass$(i - 1)
            description$(i) = tempDescription$(i - 1)
            turnIn$(i) = tempTurnIn$(i - 1)
            daysLeft$(i) = tempDaysLeft$(i - 1)
            dueDate$(i) = tempDueDate$(i - 1)
        next i
        activeTask = placement

    end sub

    'copies the main arrays into temporary ones
    sub createTempArrays
            redim tempClass$(numTasks)
            redim tempDescription$(numTasks)
            redim tempTurnIn$(numTasks)
            redim tempDaysLeft$(numTasks)
            redim tempDueDate$(numTasks)
        for i = 1 to numTasks
            tempClass$(i) = class$(i)
            tempDescription$(i) = description$(i)
            tempTurnIn$(i) = turnIn$(i)
            tempDaysLeft$(i) = daysLeft$(i)
            tempDueDate$(i) = dueDate$(i)
        next i
    end sub

    'when delete button is pressed, confirms delete
    sub delete windowhandle$
        confirm "Delete Task?" + chr$(13) + chr$(13) + class$(activeTask) + chr$(13) + description$(activeTask); response$
        if response$ = "no" then
            print #main.daysLeft, "setfocus"
            exit sub
        end if
        call deleteTask
        if activeTask > numTasks then activeTask = numTasks
        call reload
        call listBoxSelect
        if numTasks = 0 then call disable
        notice "Task Deleted"
        print #main.daysLeft, "setfocus"
    end sub

    'deletes the acitveTask
    sub deleteTask
        call addToDeleteStack
        'create temp arrays
        call createTempArrays
        'subtrack one from numTasks
        numTasks = numTasks - 1
        redim class$(numTasks)
        redim description$(numTasks)
        redim turnIn$(numTasks)
        redim daysLeft$(numTasks)
        redim dueDate$(numTasks)
        'create new arrays
        for i = 1 to activeTask - 1
            class$(i) = tempClass$(i)
            description$(i) = tempDescription$(i)
            turnIn$(i) = tempTurnIn$(i)
            daysLeft$(i) = tempDaysLeft$(i)
            dueDate$(i) = tempDueDate$(i)
        next i
        for i = activeTask to numTasks
            class$(i) = tempClass$(i + 1)
            description$(i) = tempDescription$(i + 1)
            turnIn$(i) = tempTurnIn$(i + 1)
            daysLeft$(i) = tempDaysLeft$(i + 1)
            dueDate$(i) = tempDueDate$(i + 1)
        next i
    end sub

    'edit the activeTask, now uses the add window to get rid of duplicate code
    sub edit windowhandle$
        call add "edit"
    end sub

    'save the new information into the tasks file
    sub save
        open "tasks.txt" for output as #tasks
        print #tasks, numTasks
        for i = 1 to numTasks
            print #tasks, dueDate$(i)
            print #tasks, turnIn$(i)
            print #tasks, class$(i)
            print #tasks, description$(i)
        next i
        close #tasks
    end sub

    'create a back up file when task list opened
    sub createBackUp
        open "tasksBackUp.txt" for output as #tasks
        print #tasks, numTasks
        for i = 1 to numTasks
            print #tasks, dueDate$(i)
            print #tasks, turnIn$(i)
            print #tasks, class$(i)
            print #tasks, description$(i)
        next i
        close #tasks
    end sub

    'disable buttons if no tasks
    sub disable
        print #main.viewDescription, "!disable"
        print #main.edit, "!disable"
        print #main.delete, "!disable"
    end sub

    'enable buttons tasks added (after 0 tasks exited)
    sub enable
        print #main.viewDescription, "!enable"
        print #main.edit, "!enable"
        print #main.delete, "!enable"
    end sub

    'does... well, nothing...  (But it has a purpose.  Main menu hidden buttons need to be
    'overwritten by ones that do nothing when a new window is opened to prevent errors.)
    sub doNothing windowhandle$
    end sub

    'add the activeTask to the delete stack
    'unlike other arrays in this program, deleteStack starts at index 0
    '(to make the math eaiser)
    sub addToDeleteStack

        'add more space to delete stack
        redim temp$(numDeletedTasks*4)
        for i = 0 to (numDeletedTasks*4 - 1)
            temp$(i) = deleteStack$(i)
        next i
        redim deleteStack$(numDeletedTasks*4 + 4)
        for i = 0 to (numDeletedTasks*4 - 1)
            deleteStack$(i) = temp$(i)
        next i

        numDeletedTasks = numDeletedTasks + 1

        'add new task to stack
        deleteStack$((numDeletedTasks - 1)*4) = dueDate$(activeTask)
        deleteStack$((numDeletedTasks - 1)*4 + 1) = turnIn$(activeTask)
        deleteStack$((numDeletedTasks - 1)*4 + 2) = class$(activeTask)
        deleteStack$((numDeletedTasks - 1)*4 + 3) = description$(activeTask)
    end sub

    'restore a deleted task
    sub restoreTask windowhandle$
        if numDeletedTasks = 0 then exit sub
        temp = (numDeletedTasks - 1)*4 'base index for task to add
        call addTask deleteStack$(temp), deleteStack$(temp+1), deleteStack$(temp+2), deleteStack$(temp+3)
        numDeletedTasks = numDeletedTasks - 1

        call reload
        if numTasks = 1 then call enable 'enable buttons if resotre added to empty list
        call listBoxSelect
    end sub
