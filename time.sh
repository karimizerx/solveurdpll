#!/bin/bash
function dpll_sat() {
    echo "" >result/result_sat.txt
    for i in test/sat/*.cnf; do
        echo $i >>result/result_sat.txt
        echo $i
        cat $i | wc -l >>result/result_sat.txt
        start=$(date +%s)
        ./dpll $i >>result/result_sat.txt
        end=$(date +%s)
        runtime=$(echo "$end-$start" | bc -l)
        echo "Runtime = $runtime" >>result/result_sat.txt
        echo "          > Runtime = $runtime"
    done
}

function dpll_unsat() {
    echo "" >result/result_unsat.txt
    for i in test/unsat/*.cnf; do
        echo $i >>result/result_unsat.txt
        echo $i
        cat $i | wc -l >>result/result_unsat.txt
        start=$(date +%s)
        ./dpll $i >>result/result_unsat.txt
        end=$(date +%s)
        runtime=$(echo "$end-$start" | bc -l)
        echo "Runtime = $runtime" >>result/result_unsat.txt
        echo "          > Runtime = $runtime"
    done
    echo test/unstat/bf1355-075.cnf.txt >>result/result_unsat.txt
    echo test/unstat/bf1355-075.cnf.txt
    cat test/unstat/bf1355-075.cnf.txt | wc -l >>result/result_unsat.txt
    start=$(date +%s)
    ./dpll test/unstat/bf1355-075.cnf.txt >>result/result_unsat.txt
    end=$(date +%s)
    runtime=$(echo "$end-$start" | bc -l)
    echo "Runtime = $runtime" >>result/result_unsat.txt
    echo "          > Runtime = $runtime"
}

dpll_sat
echo "              "
echo "              "
echo "              "
echo "              "
echo "              "
dpll_unsat
