DEST = ./_build/default
EXT = bc 

knights_tour:
	dune build knights_tour.${EXT}
	mv ${DEST}/knights_tour.${EXT} ./knights_tour
	chmod +w knights_tour

interact:
	dune build interact.${EXT}
	mv ${DEST}/interact.${EXT} ./interact
	chmod +w interact

build_test_storage:
	dune build test_storage.${EXT}
	mv ${DEST}/test_storage.${EXT} ./test_storage
	chmod +w test_storage

run_test_storage: build_test_storage
	./test_storage

build_test_board:
	dune build test_board.${EXT}
	mv ${DEST}/test_board.${EXT} ./test_board
	chmod +w test_board

run_test_board: build_test_board
	./test_board

build_test_search:
	dune build test_search.${EXT}
	mv ${DEST}/test_search.${EXT} ./test_search
	chmod +w test_search

run_test_search: build_test_search
	./test_search

repl:
	utop -init ./utop_init

clean:
	dune clean
	rm -f test_storage test_search test_board knights_tour interact

