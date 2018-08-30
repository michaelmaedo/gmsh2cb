program main2

    use queue_type

    type(node), pointer :: inode
    type(queue), pointer :: q

    call new_queue(q)

    allocate(inode)
    inode % data = 5
    nullify(inode % next)
    call enqueue(inode, q)

    inode % data = 6
    call enqueue(inode, q)

    inode % data = 7
    call enqueue(inode, q)

    inode % data = 15
    call enqueue(inode, q)

    inode % data = 11
    call enqueue(inode, q)

    print *, "Before deque"
    call print_queue(q)

    inode => dequeue(q)
    print *, inode % data
    call print_queue(q)

    inode = dequeue(q)
    print *, inode % data
    call print_queue(q)

    inode = dequeue(q)
    print *, inode % data
    call print_queue(q)

   inode => dequeue(q)
   print *, inode % data
   call print_queue(q)

   inode = dequeue(q)
   print *, inode % data
   call print_queue(q)

   inode = dequeue(q)
   print *, inode % data
   call print_queue(q)

   inode = dequeue(q)
   print *, inode % data
   call print_queue(q)
   inode = dequeue(q)
   print *, inode % data
   call print_queue(q)
   inode = dequeue(q)
   print *, inode % data
   call print_queue(q)
   inode = dequeue(q)
   print *, inode % data
   call print_queue(q)


   inode % data = 21
   call enqueue(inode, q)

   inode % data = 33
   call enqueue(inode, q)

   call print_queue(q)

end program main2
