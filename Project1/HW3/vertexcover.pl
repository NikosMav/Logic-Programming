:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).
:- compile(graph).

vertexcover(NNodes, Density, Cover) :-
  create_graph(NNodes, Density, Graph),  %Creating Graph (List of Edges)
  createVariables(NNodes, Nodes),  %Creating Variables
  stateConstraints(Nodes, Graph),  %Stating Constraints
  Cost #= sum(Nodes),    %Stating the cost which evaluates every state
  bb_min(search(Nodes, 0, input_order, indomain, complete, []), Cost, _),
  createCover(Nodes, Cover, 1), !.

createVariables(NNodes, Nodes) :-
  length(Nodes, NNodes),
  Nodes #:: 0..1.  %Either Node is in Cover or not

stateConstraints(_, []).
stateConstraints(Nodes, [N1 - N2| Graph]) :-
  n_th(N1, Nodes, Node1),
  n_th(N2, Nodes, Node2),
  Node1 #= 1 or Node2 #= 1,
  stateConstraints(Nodes, Graph).

createCover([], [], _).
createCover([Node|Nodes], [Counter|Cover], Counter) :-
  Node #= 1,
  NewCounter is Counter + 1,
  createCover(Nodes, Cover, NewCounter).
createCover([Node|Nodes], Cover, Counter) :-
  Node #= 0,
  NewCounter is Counter + 1,
  createCover(Nodes, Cover, NewCounter).

n_th(1, [Node| _], Node).
n_th(N, [_| Nodes], Node) :-
   N \= 1,
   N1 is N - 1,
   n_th(N1, Nodes, Node).
