% LPI MODEL: ExperimentLoopyPQWithPriors
% Description:
%{
'Experimental version of TrivialLoopyPQWithPriors.'
%}
% Sort Declarations:
%{
sort(OBJ, Unknown, { })
%}
% Random Variable Declarations:
%{
randomVariable(p, 1, OBJ, Boolean)
randomVariable(q, 1, OBJ, Boolean)
%}
% Parfactor Declarations:
%{
{{ ( on X, Y ) ([ if p(X) and q(Y) then 2 else 1 ]) }}
{{ ( on Y ) ([ if p(Y) then 0.200000000 else 0.800000000 ]) }}
{{ ( on Z ) ([ if q(Z) then 0.300000000 else 0.700000000 ]) }}
%}
%
% LBP Beliefs:
% Final Belief For: belief([ p(X0) ]) = 
% if p(X0) then 0.996016672 else 0.00398332830
%
% Belief Value at Iteration 10 = 
% if p(X0) then 0.996016637 else 0.00398336255
% Belief Value at Iteration 11 = 
% if p(X0) then 0.996016671 else 0.00398332897
% Belief Value at Iteration 12 = 
% if p(X0) then 0.996016671 else 0.00398332897
% Belief Value at Iteration 13 = 
% if p(X0) then 0.996016672 else 0.00398332833
% Belief Value at Iteration 14 = 
% if p(X0) then 0.996016672 else 0.00398332833
% Belief Value at Iteration 15 = 
% if p(X0) then 0.996016672 else 0.00398332830
%
% Final Belief For: belief([ q(X0) ]) = 
% if q(X0) then 0.997634772 else 0.00236522837
%
% Belief Value at Iteration 10 = 
% if q(X0) then 0.997634753 else 0.00236524734
% Belief Value at Iteration 11 = 
% if q(X0) then 0.997634771 else 0.00236522910
% Belief Value at Iteration 12 = 
% if q(X0) then 0.997634771 else 0.00236522910
% Belief Value at Iteration 13 = 
% if q(X0) then 0.997634772 else 0.00236522839
% Belief Value at Iteration 14 = 
% if q(X0) then 0.997634772 else 0.00236522839
% Belief Value at Iteration 15 = 
% if q(X0) then 0.997634772 else 0.00236522838
%
%
% 1 = p(obj1)
% 2 = q(obj1)
% 3 = q(obj2)
% 4 = q(obj3)
% 5 = q(obj4)
% 6 = q(obj5)
% 7 = q(obj6)
% 8 = q(obj7)
% 9 = q(obj8)
% 10 = q(obj9)
% 11 = q(obj10)
% 12 = p(obj2)
% 13 = p(obj3)
% 14 = p(obj4)
% 15 = p(obj5)
% 16 = p(obj6)
% 17 = p(obj7)
% 18 = p(obj8)
% 19 = p(obj9)
% 20 = p(obj10)
function [fg varNames] = mkExperimentLoopyPQWithPriorsFactorGraph()
    [cliques nstates] = mkExperimentLoopyPQWithPriorsCliquesAndStates();
    fg = factorGraphCreate(cliques, nstates);

    varNames = cell(20, 1);
    varNames{1} = 'p(obj1)';
    varNames{2} = 'q(obj1)';
    varNames{3} = 'q(obj2)';
    varNames{4} = 'q(obj3)';
    varNames{5} = 'q(obj4)';
    varNames{6} = 'q(obj5)';
    varNames{7} = 'q(obj6)';
    varNames{8} = 'q(obj7)';
    varNames{9} = 'q(obj8)';
    varNames{10} = 'q(obj9)';
    varNames{11} = 'q(obj10)';
    varNames{12} = 'p(obj2)';
    varNames{13} = 'p(obj3)';
    varNames{14} = 'p(obj4)';
    varNames{15} = 'p(obj5)';
    varNames{16} = 'p(obj6)';
    varNames{17} = 'p(obj7)';
    varNames{18} = 'p(obj8)';
    varNames{19} = 'p(obj9)';
    varNames{20} = 'p(obj10)';
end

function [cliques nstates] = mkExperimentLoopyPQWithPriorsCliquesAndStates()

    cliques = cell(120, 1);

    % if p(obj1) and q(obj1) then 2 else 1
    % 1 = p(obj1)
    % 2 = q(obj1)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{1} = tabularFactorCreate(T, [1 2]);

    % if p(obj1) and q(obj2) then 2 else 1
    % 1 = p(obj1)
    % 3 = q(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{2} = tabularFactorCreate(T, [1 3]);

    % if p(obj1) and q(obj3) then 2 else 1
    % 1 = p(obj1)
    % 4 = q(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{3} = tabularFactorCreate(T, [1 4]);

    % if p(obj1) and q(obj4) then 2 else 1
    % 1 = p(obj1)
    % 5 = q(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{4} = tabularFactorCreate(T, [1 5]);

    % if p(obj1) and q(obj5) then 2 else 1
    % 1 = p(obj1)
    % 6 = q(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{5} = tabularFactorCreate(T, [1 6]);

    % if p(obj1) and q(obj6) then 2 else 1
    % 1 = p(obj1)
    % 7 = q(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{6} = tabularFactorCreate(T, [1 7]);

    % if p(obj1) and q(obj7) then 2 else 1
    % 1 = p(obj1)
    % 8 = q(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{7} = tabularFactorCreate(T, [1 8]);

    % if p(obj1) and q(obj8) then 2 else 1
    % 1 = p(obj1)
    % 9 = q(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{8} = tabularFactorCreate(T, [1 9]);

    % if p(obj1) and q(obj9) then 2 else 1
    % 1 = p(obj1)
    % 10 = q(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{9} = tabularFactorCreate(T, [1 10]);

    % if p(obj1) and q(obj10) then 2 else 1
    % 1 = p(obj1)
    % 11 = q(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{10} = tabularFactorCreate(T, [1 11]);

    % if p(obj2) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{11} = tabularFactorCreate(T, [2 12]);

    % if p(obj2) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{12} = tabularFactorCreate(T, [3 12]);

    % if p(obj2) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{13} = tabularFactorCreate(T, [4 12]);

    % if p(obj2) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{14} = tabularFactorCreate(T, [5 12]);

    % if p(obj2) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{15} = tabularFactorCreate(T, [6 12]);

    % if p(obj2) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{16} = tabularFactorCreate(T, [7 12]);

    % if p(obj2) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{17} = tabularFactorCreate(T, [8 12]);

    % if p(obj2) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{18} = tabularFactorCreate(T, [9 12]);

    % if p(obj2) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{19} = tabularFactorCreate(T, [10 12]);

    % if p(obj2) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 12 = p(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{20} = tabularFactorCreate(T, [11 12]);

    % if p(obj3) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{21} = tabularFactorCreate(T, [2 13]);

    % if p(obj3) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{22} = tabularFactorCreate(T, [3 13]);

    % if p(obj3) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{23} = tabularFactorCreate(T, [4 13]);

    % if p(obj3) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{24} = tabularFactorCreate(T, [5 13]);

    % if p(obj3) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{25} = tabularFactorCreate(T, [6 13]);

    % if p(obj3) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{26} = tabularFactorCreate(T, [7 13]);

    % if p(obj3) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{27} = tabularFactorCreate(T, [8 13]);

    % if p(obj3) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{28} = tabularFactorCreate(T, [9 13]);

    % if p(obj3) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{29} = tabularFactorCreate(T, [10 13]);

    % if p(obj3) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 13 = p(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{30} = tabularFactorCreate(T, [11 13]);

    % if p(obj4) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{31} = tabularFactorCreate(T, [2 14]);

    % if p(obj4) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{32} = tabularFactorCreate(T, [3 14]);

    % if p(obj4) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{33} = tabularFactorCreate(T, [4 14]);

    % if p(obj4) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{34} = tabularFactorCreate(T, [5 14]);

    % if p(obj4) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{35} = tabularFactorCreate(T, [6 14]);

    % if p(obj4) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{36} = tabularFactorCreate(T, [7 14]);

    % if p(obj4) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{37} = tabularFactorCreate(T, [8 14]);

    % if p(obj4) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{38} = tabularFactorCreate(T, [9 14]);

    % if p(obj4) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{39} = tabularFactorCreate(T, [10 14]);

    % if p(obj4) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 14 = p(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{40} = tabularFactorCreate(T, [11 14]);

    % if p(obj5) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{41} = tabularFactorCreate(T, [2 15]);

    % if p(obj5) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{42} = tabularFactorCreate(T, [3 15]);

    % if p(obj5) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{43} = tabularFactorCreate(T, [4 15]);

    % if p(obj5) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{44} = tabularFactorCreate(T, [5 15]);

    % if p(obj5) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{45} = tabularFactorCreate(T, [6 15]);

    % if p(obj5) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{46} = tabularFactorCreate(T, [7 15]);

    % if p(obj5) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{47} = tabularFactorCreate(T, [8 15]);

    % if p(obj5) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{48} = tabularFactorCreate(T, [9 15]);

    % if p(obj5) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{49} = tabularFactorCreate(T, [10 15]);

    % if p(obj5) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 15 = p(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{50} = tabularFactorCreate(T, [11 15]);

    % if p(obj6) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{51} = tabularFactorCreate(T, [2 16]);

    % if p(obj6) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{52} = tabularFactorCreate(T, [3 16]);

    % if p(obj6) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{53} = tabularFactorCreate(T, [4 16]);

    % if p(obj6) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{54} = tabularFactorCreate(T, [5 16]);

    % if p(obj6) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{55} = tabularFactorCreate(T, [6 16]);

    % if p(obj6) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{56} = tabularFactorCreate(T, [7 16]);

    % if p(obj6) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{57} = tabularFactorCreate(T, [8 16]);

    % if p(obj6) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{58} = tabularFactorCreate(T, [9 16]);

    % if p(obj6) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{59} = tabularFactorCreate(T, [10 16]);

    % if p(obj6) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 16 = p(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{60} = tabularFactorCreate(T, [11 16]);

    % if p(obj7) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{61} = tabularFactorCreate(T, [2 17]);

    % if p(obj7) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{62} = tabularFactorCreate(T, [3 17]);

    % if p(obj7) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{63} = tabularFactorCreate(T, [4 17]);

    % if p(obj7) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{64} = tabularFactorCreate(T, [5 17]);

    % if p(obj7) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{65} = tabularFactorCreate(T, [6 17]);

    % if p(obj7) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{66} = tabularFactorCreate(T, [7 17]);

    % if p(obj7) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{67} = tabularFactorCreate(T, [8 17]);

    % if p(obj7) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{68} = tabularFactorCreate(T, [9 17]);

    % if p(obj7) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{69} = tabularFactorCreate(T, [10 17]);

    % if p(obj7) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 17 = p(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{70} = tabularFactorCreate(T, [11 17]);

    % if p(obj8) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{71} = tabularFactorCreate(T, [2 18]);

    % if p(obj8) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{72} = tabularFactorCreate(T, [3 18]);

    % if p(obj8) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{73} = tabularFactorCreate(T, [4 18]);

    % if p(obj8) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{74} = tabularFactorCreate(T, [5 18]);

    % if p(obj8) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{75} = tabularFactorCreate(T, [6 18]);

    % if p(obj8) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{76} = tabularFactorCreate(T, [7 18]);

    % if p(obj8) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{77} = tabularFactorCreate(T, [8 18]);

    % if p(obj8) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{78} = tabularFactorCreate(T, [9 18]);

    % if p(obj8) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{79} = tabularFactorCreate(T, [10 18]);

    % if p(obj8) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 18 = p(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{80} = tabularFactorCreate(T, [11 18]);

    % if p(obj9) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{81} = tabularFactorCreate(T, [2 19]);

    % if p(obj9) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{82} = tabularFactorCreate(T, [3 19]);

    % if p(obj9) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{83} = tabularFactorCreate(T, [4 19]);

    % if p(obj9) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{84} = tabularFactorCreate(T, [5 19]);

    % if p(obj9) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{85} = tabularFactorCreate(T, [6 19]);

    % if p(obj9) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{86} = tabularFactorCreate(T, [7 19]);

    % if p(obj9) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{87} = tabularFactorCreate(T, [8 19]);

    % if p(obj9) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{88} = tabularFactorCreate(T, [9 19]);

    % if p(obj9) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{89} = tabularFactorCreate(T, [10 19]);

    % if p(obj9) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 19 = p(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{90} = tabularFactorCreate(T, [11 19]);

    % if p(obj10) and q(obj1) then 2 else 1
    % 2 = q(obj1)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{91} = tabularFactorCreate(T, [2 20]);

    % if p(obj10) and q(obj2) then 2 else 1
    % 3 = q(obj2)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{92} = tabularFactorCreate(T, [3 20]);

    % if p(obj10) and q(obj3) then 2 else 1
    % 4 = q(obj3)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{93} = tabularFactorCreate(T, [4 20]);

    % if p(obj10) and q(obj4) then 2 else 1
    % 5 = q(obj4)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{94} = tabularFactorCreate(T, [5 20]);

    % if p(obj10) and q(obj5) then 2 else 1
    % 6 = q(obj5)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{95} = tabularFactorCreate(T, [6 20]);

    % if p(obj10) and q(obj6) then 2 else 1
    % 7 = q(obj6)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{96} = tabularFactorCreate(T, [7 20]);

    % if p(obj10) and q(obj7) then 2 else 1
    % 8 = q(obj7)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{97} = tabularFactorCreate(T, [8 20]);

    % if p(obj10) and q(obj8) then 2 else 1
    % 9 = q(obj8)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{98} = tabularFactorCreate(T, [9 20]);

    % if p(obj10) and q(obj9) then 2 else 1
    % 10 = q(obj9)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{99} = tabularFactorCreate(T, [10 20]);

    % if p(obj10) and q(obj10) then 2 else 1
    % 11 = q(obj10)
    % 20 = p(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 1.000000;
    T(sub2ind(size(T), 1,2)) = 1.000000;
    T(sub2ind(size(T), 2,1)) = 1.000000;
    T(sub2ind(size(T), 2,2)) = 2.000000;
    cliques{100} = tabularFactorCreate(T, [11 20]);

    % if p(obj1) then 0.200000000 else 0.800000000
    % 1 = p(obj1)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{101} = tabularFactorCreate(T, [1]);

    % if p(obj2) then 0.200000000 else 0.800000000
    % 12 = p(obj2)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{102} = tabularFactorCreate(T, [12]);

    % if p(obj3) then 0.200000000 else 0.800000000
    % 13 = p(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{103} = tabularFactorCreate(T, [13]);

    % if p(obj4) then 0.200000000 else 0.800000000
    % 14 = p(obj4)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{104} = tabularFactorCreate(T, [14]);

    % if p(obj5) then 0.200000000 else 0.800000000
    % 15 = p(obj5)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{105} = tabularFactorCreate(T, [15]);

    % if p(obj6) then 0.200000000 else 0.800000000
    % 16 = p(obj6)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{106} = tabularFactorCreate(T, [16]);

    % if p(obj7) then 0.200000000 else 0.800000000
    % 17 = p(obj7)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{107} = tabularFactorCreate(T, [17]);

    % if p(obj8) then 0.200000000 else 0.800000000
    % 18 = p(obj8)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{108} = tabularFactorCreate(T, [18]);

    % if p(obj9) then 0.200000000 else 0.800000000
    % 19 = p(obj9)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{109} = tabularFactorCreate(T, [19]);

    % if p(obj10) then 0.200000000 else 0.800000000
    % 20 = p(obj10)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.800000;
    T(sub2ind(size(T), 2)) = 0.200000;
    cliques{110} = tabularFactorCreate(T, [20]);

    % if q(obj1) then 0.300000000 else 0.700000000
    % 2 = q(obj1)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{111} = tabularFactorCreate(T, [2]);

    % if q(obj2) then 0.300000000 else 0.700000000
    % 3 = q(obj2)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{112} = tabularFactorCreate(T, [3]);

    % if q(obj3) then 0.300000000 else 0.700000000
    % 4 = q(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{113} = tabularFactorCreate(T, [4]);

    % if q(obj4) then 0.300000000 else 0.700000000
    % 5 = q(obj4)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{114} = tabularFactorCreate(T, [5]);

    % if q(obj5) then 0.300000000 else 0.700000000
    % 6 = q(obj5)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{115} = tabularFactorCreate(T, [6]);

    % if q(obj6) then 0.300000000 else 0.700000000
    % 7 = q(obj6)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{116} = tabularFactorCreate(T, [7]);

    % if q(obj7) then 0.300000000 else 0.700000000
    % 8 = q(obj7)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{117} = tabularFactorCreate(T, [8]);

    % if q(obj8) then 0.300000000 else 0.700000000
    % 9 = q(obj8)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{118} = tabularFactorCreate(T, [9]);

    % if q(obj9) then 0.300000000 else 0.700000000
    % 10 = q(obj9)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{119} = tabularFactorCreate(T, [10]);

    % if q(obj10) then 0.300000000 else 0.700000000
    % 11 = q(obj10)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{120} = tabularFactorCreate(T, [11]);

    % Note: currently all LPI random variables only have 2 states (i.e. {false, true}).
    nstates = 2*ones(20, 1);

end
