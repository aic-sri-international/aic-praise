% LPI MODEL: ExperimentLoopyParfactorsExample
% Description:
%{
'Experimental version of TrivialLoopyParfactorsExample.'
%}
% Sort Declarations:
%{
sort(OBJ, Unknown, { })
%}
% Random Variable Declarations:
%{
randomVariable(g, 1, OBJ, Boolean)
randomVariable(m, 1, OBJ, Boolean)
%}
% Parfactor Declarations:
%{
{{ ( on A, B ) ([ if g(A) and m(B) then 2 else 3 ]) | true }}
{{ ( on B, A ) ([ if g(B) and m(A) then 4 else 5 ]) | true }}
%}
%
% LBP Beliefs:
% Final Belief For: belief([ g(X0) ]) = 
% if g(X0) then 0.218812743 else 0.781187257
%
% Belief Value at Iteration 10 = 
% if g(X0) then 0.103394195 else 0.896605805
% Belief Value at Iteration 11 = 
% if g(X0) then 0.349439326 else 0.650560674
% Belief Value at Iteration 12 = 
% if g(X0) then 0.349439326 else 0.650560674
% Belief Value at Iteration 13 = 
% if g(X0) then 0.118340384 else 0.881659616
% Belief Value at Iteration 14 = 
% if g(X0) then 0.118340384 else 0.881659616
% Belief Value at Iteration 15 = 
% if g(X0) then 0.330339845 else 0.669660155
% Belief Value at Iteration 500 = 
% if g(X0) then 0.218815867 else 0.781184133
% Belief Value at Iteration 1000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 1500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 2000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 2500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 3000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 3500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 4000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 4500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 5000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 5500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 6000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 6500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 7000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 7500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 8000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 8500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 9000 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 9500 = 
% if g(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 10000 = 
% if g(X0) then 0.218812743 else 0.781187257
%
% Final Belief For: belief([ m(X0) ]) = 
% if m(X0) then 0.218812743 else 0.781187257
%
% Belief Value at Iteration 10 = 
% if m(X0) then 0.103394195 else 0.896605805
% Belief Value at Iteration 11 = 
% if m(X0) then 0.349439326 else 0.650560674
% Belief Value at Iteration 12 = 
% if m(X0) then 0.349439326 else 0.650560674
% Belief Value at Iteration 13 = 
% if m(X0) then 0.118340384 else 0.881659616
% Belief Value at Iteration 14 = 
% if m(X0) then 0.118340384 else 0.881659616
% Belief Value at Iteration 15 = 
% if m(X0) then 0.330339845 else 0.669660155
% Belief Value at Iteration 500 = 
% if m(X0) then 0.218815867 else 0.781184133
% Belief Value at Iteration 1000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 1500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 2000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 2500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 3000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 3500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 4000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 4500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 5000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 5500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 6000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 6500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 7000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 7500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 8000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 8500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 9000 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 9500 = 
% if m(X0) then 0.218812743 else 0.781187257
% Belief Value at Iteration 10000 = 
% if m(X0) then 0.218812743 else 0.781187257
%
%
% 1 = g(obj1)
% 2 = m(obj1)
% 3 = m(obj2)
% 4 = m(obj3)
% 5 = m(obj4)
% 6 = m(obj5)
% 7 = m(obj6)
% 8 = m(obj7)
% 9 = m(obj8)
% 10 = m(obj9)
% 11 = m(obj10)
% 12 = g(obj2)
% 13 = g(obj3)
% 14 = g(obj4)
% 15 = g(obj5)
% 16 = g(obj6)
% 17 = g(obj7)
% 18 = g(obj8)
% 19 = g(obj9)
% 20 = g(obj10)
function [fg varNames] = mkExperimentLoopyParfactorsExampleFactorGraph()
    [cliques nstates] = mkExperimentLoopyParfactorsExampleCliquesAndStates();
    fg = factorGraphCreate(cliques, nstates);

    varNames = cell(20, 1);
    varNames{1} = 'g(obj1)';
    varNames{2} = 'm(obj1)';
    varNames{3} = 'm(obj2)';
    varNames{4} = 'm(obj3)';
    varNames{5} = 'm(obj4)';
    varNames{6} = 'm(obj5)';
    varNames{7} = 'm(obj6)';
    varNames{8} = 'm(obj7)';
    varNames{9} = 'm(obj8)';
    varNames{10} = 'm(obj9)';
    varNames{11} = 'm(obj10)';
    varNames{12} = 'g(obj2)';
    varNames{13} = 'g(obj3)';
    varNames{14} = 'g(obj4)';
    varNames{15} = 'g(obj5)';
    varNames{16} = 'g(obj6)';
    varNames{17} = 'g(obj7)';
    varNames{18} = 'g(obj8)';
    varNames{19} = 'g(obj9)';
    varNames{20} = 'g(obj10)';
end

function [cliques nstates] = mkExperimentLoopyParfactorsExampleCliquesAndStates()

    cliques = cell(120, 1);

    % if g(obj1) and m(obj1) then 2 else 3
    % if g(obj1) and m(obj1) then 4 else 5
    % 1 = g(obj1)
    % 2 = m(obj1)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{1} = tabularFactorCreate(T, [1 2]);

    % if g(obj1) and m(obj2) then 2 else 3
    % if g(obj1) and m(obj2) then 4 else 5
    % 1 = g(obj1)
    % 3 = m(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{2} = tabularFactorCreate(T, [1 3]);

    % if g(obj1) and m(obj3) then 2 else 3
    % if g(obj1) and m(obj3) then 4 else 5
    % 1 = g(obj1)
    % 4 = m(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{3} = tabularFactorCreate(T, [1 4]);

    % if g(obj1) and m(obj4) then 2 else 3
    % if g(obj1) and m(obj4) then 4 else 5
    % 1 = g(obj1)
    % 5 = m(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{4} = tabularFactorCreate(T, [1 5]);

    % if g(obj1) and m(obj5) then 2 else 3
    % if g(obj1) and m(obj5) then 4 else 5
    % 1 = g(obj1)
    % 6 = m(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{5} = tabularFactorCreate(T, [1 6]);

    % if g(obj1) and m(obj6) then 2 else 3
    % if g(obj1) and m(obj6) then 4 else 5
    % 1 = g(obj1)
    % 7 = m(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{6} = tabularFactorCreate(T, [1 7]);

    % if g(obj1) and m(obj7) then 2 else 3
    % if g(obj1) and m(obj7) then 4 else 5
    % 1 = g(obj1)
    % 8 = m(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{7} = tabularFactorCreate(T, [1 8]);

    % if g(obj1) and m(obj8) then 2 else 3
    % if g(obj1) and m(obj8) then 4 else 5
    % 1 = g(obj1)
    % 9 = m(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{8} = tabularFactorCreate(T, [1 9]);

    % if g(obj1) and m(obj9) then 2 else 3
    % if g(obj1) and m(obj9) then 4 else 5
    % 1 = g(obj1)
    % 10 = m(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{9} = tabularFactorCreate(T, [1 10]);

    % if g(obj1) and m(obj10) then 2 else 3
    % if g(obj1) and m(obj10) then 4 else 5
    % 1 = g(obj1)
    % 11 = m(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{10} = tabularFactorCreate(T, [1 11]);

    % if g(obj2) and m(obj1) then 2 else 3
    % if g(obj2) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{11} = tabularFactorCreate(T, [2 12]);

    % if g(obj2) and m(obj2) then 2 else 3
    % if g(obj2) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{12} = tabularFactorCreate(T, [3 12]);

    % if g(obj2) and m(obj3) then 2 else 3
    % if g(obj2) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{13} = tabularFactorCreate(T, [4 12]);

    % if g(obj2) and m(obj4) then 2 else 3
    % if g(obj2) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{14} = tabularFactorCreate(T, [5 12]);

    % if g(obj2) and m(obj5) then 2 else 3
    % if g(obj2) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{15} = tabularFactorCreate(T, [6 12]);

    % if g(obj2) and m(obj6) then 2 else 3
    % if g(obj2) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{16} = tabularFactorCreate(T, [7 12]);

    % if g(obj2) and m(obj7) then 2 else 3
    % if g(obj2) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{17} = tabularFactorCreate(T, [8 12]);

    % if g(obj2) and m(obj8) then 2 else 3
    % if g(obj2) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{18} = tabularFactorCreate(T, [9 12]);

    % if g(obj2) and m(obj9) then 2 else 3
    % if g(obj2) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{19} = tabularFactorCreate(T, [10 12]);

    % if g(obj2) and m(obj10) then 2 else 3
    % if g(obj2) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 12 = g(obj2)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{20} = tabularFactorCreate(T, [11 12]);

    % if g(obj3) and m(obj1) then 2 else 3
    % if g(obj3) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{21} = tabularFactorCreate(T, [2 13]);

    % if g(obj3) and m(obj2) then 2 else 3
    % if g(obj3) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{22} = tabularFactorCreate(T, [3 13]);

    % if g(obj3) and m(obj3) then 2 else 3
    % if g(obj3) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{23} = tabularFactorCreate(T, [4 13]);

    % if g(obj3) and m(obj4) then 2 else 3
    % if g(obj3) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{24} = tabularFactorCreate(T, [5 13]);

    % if g(obj3) and m(obj5) then 2 else 3
    % if g(obj3) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{25} = tabularFactorCreate(T, [6 13]);

    % if g(obj3) and m(obj6) then 2 else 3
    % if g(obj3) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{26} = tabularFactorCreate(T, [7 13]);

    % if g(obj3) and m(obj7) then 2 else 3
    % if g(obj3) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{27} = tabularFactorCreate(T, [8 13]);

    % if g(obj3) and m(obj8) then 2 else 3
    % if g(obj3) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{28} = tabularFactorCreate(T, [9 13]);

    % if g(obj3) and m(obj9) then 2 else 3
    % if g(obj3) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{29} = tabularFactorCreate(T, [10 13]);

    % if g(obj3) and m(obj10) then 2 else 3
    % if g(obj3) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 13 = g(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{30} = tabularFactorCreate(T, [11 13]);

    % if g(obj4) and m(obj1) then 2 else 3
    % if g(obj4) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{31} = tabularFactorCreate(T, [2 14]);

    % if g(obj4) and m(obj2) then 2 else 3
    % if g(obj4) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{32} = tabularFactorCreate(T, [3 14]);

    % if g(obj4) and m(obj3) then 2 else 3
    % if g(obj4) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{33} = tabularFactorCreate(T, [4 14]);

    % if g(obj4) and m(obj4) then 2 else 3
    % if g(obj4) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{34} = tabularFactorCreate(T, [5 14]);

    % if g(obj4) and m(obj5) then 2 else 3
    % if g(obj4) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{35} = tabularFactorCreate(T, [6 14]);

    % if g(obj4) and m(obj6) then 2 else 3
    % if g(obj4) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{36} = tabularFactorCreate(T, [7 14]);

    % if g(obj4) and m(obj7) then 2 else 3
    % if g(obj4) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{37} = tabularFactorCreate(T, [8 14]);

    % if g(obj4) and m(obj8) then 2 else 3
    % if g(obj4) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{38} = tabularFactorCreate(T, [9 14]);

    % if g(obj4) and m(obj9) then 2 else 3
    % if g(obj4) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{39} = tabularFactorCreate(T, [10 14]);

    % if g(obj4) and m(obj10) then 2 else 3
    % if g(obj4) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 14 = g(obj4)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{40} = tabularFactorCreate(T, [11 14]);

    % if g(obj5) and m(obj1) then 2 else 3
    % if g(obj5) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{41} = tabularFactorCreate(T, [2 15]);

    % if g(obj5) and m(obj2) then 2 else 3
    % if g(obj5) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{42} = tabularFactorCreate(T, [3 15]);

    % if g(obj5) and m(obj3) then 2 else 3
    % if g(obj5) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{43} = tabularFactorCreate(T, [4 15]);

    % if g(obj5) and m(obj4) then 2 else 3
    % if g(obj5) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{44} = tabularFactorCreate(T, [5 15]);

    % if g(obj5) and m(obj5) then 2 else 3
    % if g(obj5) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{45} = tabularFactorCreate(T, [6 15]);

    % if g(obj5) and m(obj6) then 2 else 3
    % if g(obj5) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{46} = tabularFactorCreate(T, [7 15]);

    % if g(obj5) and m(obj7) then 2 else 3
    % if g(obj5) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{47} = tabularFactorCreate(T, [8 15]);

    % if g(obj5) and m(obj8) then 2 else 3
    % if g(obj5) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{48} = tabularFactorCreate(T, [9 15]);

    % if g(obj5) and m(obj9) then 2 else 3
    % if g(obj5) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{49} = tabularFactorCreate(T, [10 15]);

    % if g(obj5) and m(obj10) then 2 else 3
    % if g(obj5) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 15 = g(obj5)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{50} = tabularFactorCreate(T, [11 15]);

    % if g(obj6) and m(obj1) then 2 else 3
    % if g(obj6) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{51} = tabularFactorCreate(T, [2 16]);

    % if g(obj6) and m(obj2) then 2 else 3
    % if g(obj6) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{52} = tabularFactorCreate(T, [3 16]);

    % if g(obj6) and m(obj3) then 2 else 3
    % if g(obj6) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{53} = tabularFactorCreate(T, [4 16]);

    % if g(obj6) and m(obj4) then 2 else 3
    % if g(obj6) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{54} = tabularFactorCreate(T, [5 16]);

    % if g(obj6) and m(obj5) then 2 else 3
    % if g(obj6) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{55} = tabularFactorCreate(T, [6 16]);

    % if g(obj6) and m(obj6) then 2 else 3
    % if g(obj6) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{56} = tabularFactorCreate(T, [7 16]);

    % if g(obj6) and m(obj7) then 2 else 3
    % if g(obj6) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{57} = tabularFactorCreate(T, [8 16]);

    % if g(obj6) and m(obj8) then 2 else 3
    % if g(obj6) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{58} = tabularFactorCreate(T, [9 16]);

    % if g(obj6) and m(obj9) then 2 else 3
    % if g(obj6) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{59} = tabularFactorCreate(T, [10 16]);

    % if g(obj6) and m(obj10) then 2 else 3
    % if g(obj6) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 16 = g(obj6)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{60} = tabularFactorCreate(T, [11 16]);

    % if g(obj7) and m(obj1) then 2 else 3
    % if g(obj7) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{61} = tabularFactorCreate(T, [2 17]);

    % if g(obj7) and m(obj2) then 2 else 3
    % if g(obj7) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{62} = tabularFactorCreate(T, [3 17]);

    % if g(obj7) and m(obj3) then 2 else 3
    % if g(obj7) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{63} = tabularFactorCreate(T, [4 17]);

    % if g(obj7) and m(obj4) then 2 else 3
    % if g(obj7) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{64} = tabularFactorCreate(T, [5 17]);

    % if g(obj7) and m(obj5) then 2 else 3
    % if g(obj7) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{65} = tabularFactorCreate(T, [6 17]);

    % if g(obj7) and m(obj6) then 2 else 3
    % if g(obj7) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{66} = tabularFactorCreate(T, [7 17]);

    % if g(obj7) and m(obj7) then 2 else 3
    % if g(obj7) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{67} = tabularFactorCreate(T, [8 17]);

    % if g(obj7) and m(obj8) then 2 else 3
    % if g(obj7) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{68} = tabularFactorCreate(T, [9 17]);

    % if g(obj7) and m(obj9) then 2 else 3
    % if g(obj7) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{69} = tabularFactorCreate(T, [10 17]);

    % if g(obj7) and m(obj10) then 2 else 3
    % if g(obj7) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 17 = g(obj7)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{70} = tabularFactorCreate(T, [11 17]);

    % if g(obj8) and m(obj1) then 2 else 3
    % if g(obj8) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{71} = tabularFactorCreate(T, [2 18]);

    % if g(obj8) and m(obj2) then 2 else 3
    % if g(obj8) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{72} = tabularFactorCreate(T, [3 18]);

    % if g(obj8) and m(obj3) then 2 else 3
    % if g(obj8) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{73} = tabularFactorCreate(T, [4 18]);

    % if g(obj8) and m(obj4) then 2 else 3
    % if g(obj8) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{74} = tabularFactorCreate(T, [5 18]);

    % if g(obj8) and m(obj5) then 2 else 3
    % if g(obj8) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{75} = tabularFactorCreate(T, [6 18]);

    % if g(obj8) and m(obj6) then 2 else 3
    % if g(obj8) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{76} = tabularFactorCreate(T, [7 18]);

    % if g(obj8) and m(obj7) then 2 else 3
    % if g(obj8) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{77} = tabularFactorCreate(T, [8 18]);

    % if g(obj8) and m(obj8) then 2 else 3
    % if g(obj8) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{78} = tabularFactorCreate(T, [9 18]);

    % if g(obj8) and m(obj9) then 2 else 3
    % if g(obj8) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{79} = tabularFactorCreate(T, [10 18]);

    % if g(obj8) and m(obj10) then 2 else 3
    % if g(obj8) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 18 = g(obj8)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{80} = tabularFactorCreate(T, [11 18]);

    % if g(obj9) and m(obj1) then 2 else 3
    % if g(obj9) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{81} = tabularFactorCreate(T, [2 19]);

    % if g(obj9) and m(obj2) then 2 else 3
    % if g(obj9) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{82} = tabularFactorCreate(T, [3 19]);

    % if g(obj9) and m(obj3) then 2 else 3
    % if g(obj9) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{83} = tabularFactorCreate(T, [4 19]);

    % if g(obj9) and m(obj4) then 2 else 3
    % if g(obj9) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{84} = tabularFactorCreate(T, [5 19]);

    % if g(obj9) and m(obj5) then 2 else 3
    % if g(obj9) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{85} = tabularFactorCreate(T, [6 19]);

    % if g(obj9) and m(obj6) then 2 else 3
    % if g(obj9) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{86} = tabularFactorCreate(T, [7 19]);

    % if g(obj9) and m(obj7) then 2 else 3
    % if g(obj9) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{87} = tabularFactorCreate(T, [8 19]);

    % if g(obj9) and m(obj8) then 2 else 3
    % if g(obj9) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{88} = tabularFactorCreate(T, [9 19]);

    % if g(obj9) and m(obj9) then 2 else 3
    % if g(obj9) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{89} = tabularFactorCreate(T, [10 19]);

    % if g(obj9) and m(obj10) then 2 else 3
    % if g(obj9) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 19 = g(obj9)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{90} = tabularFactorCreate(T, [11 19]);

    % if g(obj10) and m(obj1) then 2 else 3
    % if g(obj10) and m(obj1) then 4 else 5
    % 2 = m(obj1)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{91} = tabularFactorCreate(T, [2 20]);

    % if g(obj10) and m(obj2) then 2 else 3
    % if g(obj10) and m(obj2) then 4 else 5
    % 3 = m(obj2)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{92} = tabularFactorCreate(T, [3 20]);

    % if g(obj10) and m(obj3) then 2 else 3
    % if g(obj10) and m(obj3) then 4 else 5
    % 4 = m(obj3)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{93} = tabularFactorCreate(T, [4 20]);

    % if g(obj10) and m(obj4) then 2 else 3
    % if g(obj10) and m(obj4) then 4 else 5
    % 5 = m(obj4)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{94} = tabularFactorCreate(T, [5 20]);

    % if g(obj10) and m(obj5) then 2 else 3
    % if g(obj10) and m(obj5) then 4 else 5
    % 6 = m(obj5)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{95} = tabularFactorCreate(T, [6 20]);

    % if g(obj10) and m(obj6) then 2 else 3
    % if g(obj10) and m(obj6) then 4 else 5
    % 7 = m(obj6)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{96} = tabularFactorCreate(T, [7 20]);

    % if g(obj10) and m(obj7) then 2 else 3
    % if g(obj10) and m(obj7) then 4 else 5
    % 8 = m(obj7)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{97} = tabularFactorCreate(T, [8 20]);

    % if g(obj10) and m(obj8) then 2 else 3
    % if g(obj10) and m(obj8) then 4 else 5
    % 9 = m(obj8)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{98} = tabularFactorCreate(T, [9 20]);

    % if g(obj10) and m(obj9) then 2 else 3
    % if g(obj10) and m(obj9) then 4 else 5
    % 10 = m(obj9)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{99} = tabularFactorCreate(T, [10 20]);

    % if g(obj10) and m(obj10) then 2 else 3
    % if g(obj10) and m(obj10) then 4 else 5
    % 11 = m(obj10)
    % 20 = g(obj10)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 15.000000;
    T(sub2ind(size(T), 1,2)) = 15.000000;
    T(sub2ind(size(T), 2,1)) = 15.000000;
    T(sub2ind(size(T), 2,2)) = 8.000000;
    cliques{100} = tabularFactorCreate(T, [11 20]);

    % g(obj1)
    % 1 = g(obj1)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{101} = tabularFactorCreate(T, [1]);

    % m(obj1)
    % 2 = m(obj1)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{102} = tabularFactorCreate(T, [2]);

    % m(obj2)
    % 3 = m(obj2)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{103} = tabularFactorCreate(T, [3]);

    % m(obj3)
    % 4 = m(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{104} = tabularFactorCreate(T, [4]);

    % m(obj4)
    % 5 = m(obj4)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{105} = tabularFactorCreate(T, [5]);

    % m(obj5)
    % 6 = m(obj5)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{106} = tabularFactorCreate(T, [6]);

    % m(obj6)
    % 7 = m(obj6)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{107} = tabularFactorCreate(T, [7]);

    % m(obj7)
    % 8 = m(obj7)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{108} = tabularFactorCreate(T, [8]);

    % m(obj8)
    % 9 = m(obj8)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{109} = tabularFactorCreate(T, [9]);

    % m(obj9)
    % 10 = m(obj9)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{110} = tabularFactorCreate(T, [10]);

    % m(obj10)
    % 11 = m(obj10)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{111} = tabularFactorCreate(T, [11]);

    % g(obj2)
    % 12 = g(obj2)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{112} = tabularFactorCreate(T, [12]);

    % g(obj3)
    % 13 = g(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{113} = tabularFactorCreate(T, [13]);

    % g(obj4)
    % 14 = g(obj4)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{114} = tabularFactorCreate(T, [14]);

    % g(obj5)
    % 15 = g(obj5)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{115} = tabularFactorCreate(T, [15]);

    % g(obj6)
    % 16 = g(obj6)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{116} = tabularFactorCreate(T, [16]);

    % g(obj7)
    % 17 = g(obj7)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{117} = tabularFactorCreate(T, [17]);

    % g(obj8)
    % 18 = g(obj8)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{118} = tabularFactorCreate(T, [18]);

    % g(obj9)
    % 19 = g(obj9)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{119} = tabularFactorCreate(T, [19]);

    % g(obj10)
    % 20 = g(obj10)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{120} = tabularFactorCreate(T, [20]);

    % Note: currently all LPI random variables only have 2 states (i.e. {false, true}).
    nstates = 2*ones(20, 1);

end
