#!/usr/bin/env python

import sys
import glob
import json
from graphviz import Digraph

dot = Digraph(comment='Bonus dependencies')

for problem_file in glob.glob("*.problem"):
    problem_no = problem_file.split(".")[0]
    
    with open(problem_file) as problem:
        problemJson = json.load(problem)
        bonuses = problemJson["bonuses"]

        for bonus in bonuses:
            bonusType = bonus["bonus"]
            enablesProblem = bonus["problem"] 

            dot.edge(str(problem_no), str(enablesProblem))

dot.render('deps.gv', view=True) 