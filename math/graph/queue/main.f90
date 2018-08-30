program main

    use graph_stru
    implicit none

    integer :: V = 5
    type(graph), pointer :: G
    G => new_empty_graph(V)
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
    call print_graph(G)
    call print_deg(G)

end program main
