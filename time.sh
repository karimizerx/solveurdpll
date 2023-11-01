#!/bin/bash
function dpll() {
    echo "" >result/result_.txt
    echo $1 >>result/result_.txt
    echo $1
    cat $1 | wc -l >>result/result_.txt
    start=$(date +%s)
    ./dpll $1 >>result/result_.txt
    end=$(date +%s)
    runtime=$(echo "$end-$start" | bc -l)
    echo "Runtime = $runtime" >>result/result_.txt
    echo "          > Runtime = $runtime"
}

dpll test/unsat/exemple-3-13.cnf
dpll test/unsat/exemple-7-4.cnf
dpll test/unsat/grammaire.cnf
dpll test/unsat/hole6.cnf
dpll test/unsat/systeme.cnf
dpll test/sat/accessibilite.cnf
dpll test/sat/coloriage.cnf
dpll test/sat/dependances.cnf
dpll test/sat/exemple-5-8.cnf
dpll test/sat/exemple-7-2.cnf
dpll test/sat/exemple-7-8.cnf
dpll test/sat/flat50-1000.cnf
dpll test/sat/ii8a2.cnf
dpll test/sat/peirce.cnf
dpll test/sat/sudoku-4x4.cnf
dpll test/sat/sudoku-9x9-medium.cnf
echo "              "
dpll test/sat/tictactoe.cnf
dpll test/sat/sudoku-9x9-easy.cnf
dpll test/sat/sudoku-9x9-expert.cnf
dpll test/sat/sudoku-9x9-hard.cnf
echo "              "
# dpll test/unsat/dubois20.cnf
# dpll test/unsat/dubois21.cnf
# dpll test/sat/ais12.cnf
# dpll test/unsat/bf1355-075.cnf
# dpll test/unsat/aim-50-1_6-no-1.cnf
# dpll test/sat/sudoku-9x9-god.cnf
# dpll test/sat/hamilton.cnf.txt
