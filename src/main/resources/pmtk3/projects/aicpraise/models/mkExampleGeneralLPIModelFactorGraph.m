% LPI MODEL: ExampleGeneralLPIModel
% Description:
%{
'Example General LPI Model for conversion to PMTK3 factors.'
%}
% Sort Declarations:
%{
sort(OBJ, Unknown, { a, b })
%}
% Random Variable Declarations:
%{
randomVariable(p, 1, OBJ, Boolean)
randomVariable(q, 1, OBJ, Boolean)
randomVariable(r, 1, OBJ, Boolean)
%}
% Parfactor Declarations:
%{
{{ ( on X, Y ) ([ if p(X) and r(Y) then if q(X) then 0.600000000 else 0.400000000 else if q(X) then 0.550000000 else 0.450000000 ]) }}
%}
%
% LBP Beliefs:
% belief([ p(X0) ]) = 
% if p(X0) then 0.509103990 else 0.490896010
% belief([ q(X0) ]) = 
% if q(X0) then 0.680846690 else 0.319153310
% belief([ r(X0) ]) = 
% if r(X0) then 0.509103990 else 0.490896010
%
% 1 = p(a)
% 2 = r(a)
% 3 = q(a)
% 4 = r(b)
% 5 = r(obj3)
% 6 = p(b)
% 7 = q(b)
% 8 = p(obj3)
% 9 = q(obj3)
function [fg varNames] = mkExampleGeneralLPIModelFactorGraph()
    [cliques nstates] = mkExampleGeneralLPIModelCliquesAndStates();
    fg = factorGraphCreate(cliques, nstates);

    varNames = cell(9, 1);
    varNames{1} = 'p(a)';
    varNames{2} = 'r(a)';
    varNames{3} = 'q(a)';
    varNames{4} = 'r(b)';
    varNames{5} = 'r(obj3)';
    varNames{6} = 'p(b)';
    varNames{7} = 'q(b)';
    varNames{8} = 'p(obj3)';
    varNames{9} = 'q(obj3)';
end

function [cliques nstates] = mkExampleGeneralLPIModelCliquesAndStates()

    cliques = cell(18, 1);

    % if p(a) and r(a) then if q(a) then 0.600000000 else 0.400000000 else if q(a) then 0.550000000 else 0.450000000
    % 1 = p(a)
    % 2 = r(a)
    % 3 = q(a)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.550000;
    T(sub2ind(size(T), 1,2,1)) = 0.450000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.550000;
    T(sub2ind(size(T), 2,2,1)) = 0.400000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{1} = tabularFactorCreate(T, [1 2 3]);

    % if p(a) and r(b) then if q(a) then 0.600000000 else 0.400000000 else if q(a) then 0.550000000 else 0.450000000
    % 1 = p(a)
    % 3 = q(a)
    % 4 = r(b)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.450000;
    T(sub2ind(size(T), 1,2,1)) = 0.550000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.400000;
    T(sub2ind(size(T), 2,2,1)) = 0.550000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{2} = tabularFactorCreate(T, [1 3 4]);

    % if p(a) and r(obj3) then if q(a) then 0.600000000 else 0.400000000 else if q(a) then 0.550000000 else 0.450000000
    % 1 = p(a)
    % 3 = q(a)
    % 5 = r(obj3)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.450000;
    T(sub2ind(size(T), 1,2,1)) = 0.550000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.400000;
    T(sub2ind(size(T), 2,2,1)) = 0.550000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{3} = tabularFactorCreate(T, [1 3 5]);

    % if p(b) and r(a) then if q(b) then 0.600000000 else 0.400000000 else if q(b) then 0.550000000 else 0.450000000
    % 2 = r(a)
    % 6 = p(b)
    % 7 = q(b)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.550000;
    T(sub2ind(size(T), 1,2,1)) = 0.450000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.550000;
    T(sub2ind(size(T), 2,2,1)) = 0.400000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{4} = tabularFactorCreate(T, [2 6 7]);

    % if p(b) and r(b) then if q(b) then 0.600000000 else 0.400000000 else if q(b) then 0.550000000 else 0.450000000
    % 4 = r(b)
    % 6 = p(b)
    % 7 = q(b)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.550000;
    T(sub2ind(size(T), 1,2,1)) = 0.450000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.550000;
    T(sub2ind(size(T), 2,2,1)) = 0.400000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{5} = tabularFactorCreate(T, [4 6 7]);

    % if p(b) and r(obj3) then if q(b) then 0.600000000 else 0.400000000 else if q(b) then 0.550000000 else 0.450000000
    % 5 = r(obj3)
    % 6 = p(b)
    % 7 = q(b)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.550000;
    T(sub2ind(size(T), 1,2,1)) = 0.450000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.550000;
    T(sub2ind(size(T), 2,2,1)) = 0.400000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{6} = tabularFactorCreate(T, [5 6 7]);

    % if p(obj3) and r(a) then if q(obj3) then 0.600000000 else 0.400000000 else if q(obj3) then 0.550000000 else 0.450000000
    % 2 = r(a)
    % 8 = p(obj3)
    % 9 = q(obj3)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.550000;
    T(sub2ind(size(T), 1,2,1)) = 0.450000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.550000;
    T(sub2ind(size(T), 2,2,1)) = 0.400000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{7} = tabularFactorCreate(T, [2 8 9]);

    % if p(obj3) and r(b) then if q(obj3) then 0.600000000 else 0.400000000 else if q(obj3) then 0.550000000 else 0.450000000
    % 4 = r(b)
    % 8 = p(obj3)
    % 9 = q(obj3)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.550000;
    T(sub2ind(size(T), 1,2,1)) = 0.450000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.550000;
    T(sub2ind(size(T), 2,2,1)) = 0.400000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{8} = tabularFactorCreate(T, [4 8 9]);

    % if p(obj3) and r(obj3) then if q(obj3) then 0.600000000 else 0.400000000 else if q(obj3) then 0.550000000 else 0.450000000
    % 5 = r(obj3)
    % 8 = p(obj3)
    % 9 = q(obj3)
    T = reshape(zeros(1, 8), 2, 2, 2);
    T(sub2ind(size(T), 1,1,1)) = 0.450000;
    T(sub2ind(size(T), 1,1,2)) = 0.550000;
    T(sub2ind(size(T), 1,2,1)) = 0.450000;
    T(sub2ind(size(T), 1,2,2)) = 0.550000;
    T(sub2ind(size(T), 2,1,1)) = 0.450000;
    T(sub2ind(size(T), 2,1,2)) = 0.550000;
    T(sub2ind(size(T), 2,2,1)) = 0.400000;
    T(sub2ind(size(T), 2,2,2)) = 0.600000;
    cliques{9} = tabularFactorCreate(T, [5 8 9]);

    % p(a)
    % 1 = p(a)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{10} = tabularFactorCreate(T, [1]);

    % r(a)
    % 2 = r(a)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{11} = tabularFactorCreate(T, [2]);

    % q(a)
    % 3 = q(a)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{12} = tabularFactorCreate(T, [3]);

    % r(b)
    % 4 = r(b)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{13} = tabularFactorCreate(T, [4]);

    % r(obj3)
    % 5 = r(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{14} = tabularFactorCreate(T, [5]);

    % p(b)
    % 6 = p(b)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{15} = tabularFactorCreate(T, [6]);

    % q(b)
    % 7 = q(b)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{16} = tabularFactorCreate(T, [7]);

    % p(obj3)
    % 8 = p(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{17} = tabularFactorCreate(T, [8]);

    % q(obj3)
    % 9 = q(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{18} = tabularFactorCreate(T, [9]);

    % Note: currently all LPI random variables only have 2 states (i.e. {false, true}).
    nstates = 2*ones(9, 1);

end
