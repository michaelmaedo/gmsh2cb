program main

    use graph_stru
    use queue_type
    implicit none

    integer :: V = 5
    type(graph), pointer :: G
    type(queue), pointer :: q
    G => new_empty_graph(V)
    call new_queue(q)
    call add_vertx(G, 1, 2)
    call add_vertx(G, 1, 5)
    call add_vertx(G, 2, 1)
    call add_vertx(G, 2, 5)
    call add_vertx(G, 2, 4)
    call add_vertx(G, 2, 3)
    call add_vertx(G, 3, 2)
    call add_vertx(G, 3, 4)
    call add_vertx(G, 4, 5)
    call add_vertx(G, 4, 2)
    call add_vertx(G, 4, 3)
    call add_vertx(G, 5, 1)
    call add_vertx(G, 5, 2)
    call add_vertx(G, 5, 4)
    call sort(G, 1)
    call sort(G, 2)
    call sort(G, 3)
    call sort(G, 4)
    call sort(G, 5)
    call print_graph(G)
    print *
!    call print_deg(G)
    call enqueue(G % adj(2) % head, q)
    call print_queue(q)

end program main
