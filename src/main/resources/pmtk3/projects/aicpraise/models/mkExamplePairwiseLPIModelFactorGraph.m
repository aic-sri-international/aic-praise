% LPI MODEL: ExamplePairwiseLPIModel
% Description:
%{
'Example Pairwise LPI Model for conversion to PMTK3 factors.'
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
{{ ( on X ) ([ if p(X) then if q(X) then 0.600000000 else 0.400000000 else if q(X) then 0.550000000 else 0.450000000 ]) }}
{{ ( on Y ) ([ if p(Y) and q(Y) then 0.200000000 else 0.800000000 ]) }}
{{ ( on Z ) ([ if q(Z) then 0.100000000 else 0.900000000 ]) }}
{{ ( on T ) ([ if r(T) then 0.300000000 else 0.700000000 ]) }}
{ ([ if r(a) and p(a) then 0.250000000 else 0.750000000 ]), ([ if r(b) then if p(b) then 0.650000000 else 0.350000000 else if p(b) then 0.640000000 else 0.360000000 ]) }
%}
%
% LBP Beliefs:
% belief([ p(X0) ]) = 
% if X0 = a then if p(a) then 0.395707346 else 0.604292654 else if X0 = b then if p(b) then 0.593981510 else 0.406018490 else if p(X0) then 0.449587506 else 0.550412494
% belief([ q(X0) ]) = 
% if X0 = a then if q(a) then 0.0919550461 else 0.908044954 else if X0 = b then if q(b) then 0.0762125254 else 0.923787475 else if q(X0) then 0.0878036854 else 0.912196315
% belief([ r(X0) ]) = 
% if X0 = a then if r(a) then 0.230751743 else 0.769248257 else if X0 = b then if r(b) then 0.299551552 else 0.700448448 else if r(X0) then 0.300000000 else 0.700000000
%
% 1 = p(a)
% 2 = q(a)
% 3 = p(b)
% 4 = q(b)
% 5 = p(obj3)
% 6 = q(obj3)
% 7 = r(a)
% 8 = r(b)
% 9 = r(obj3)
function [fg varNames] = mkExamplePairwiseLPIModelFactorGraph()
    [cliques nstates] = mkExamplePairwiseLPIModelCliquesAndStates();
    fg = factorGraphCreate(cliques, nstates);

    varNames = cell(9, 1);
    varNames{1} = 'p(a)';
    varNames{2} = 'q(a)';
    varNames{3} = 'p(b)';
    varNames{4} = 'q(b)';
    varNames{5} = 'p(obj3)';
    varNames{6} = 'q(obj3)';
    varNames{7} = 'r(a)';
    varNames{8} = 'r(b)';
    varNames{9} = 'r(obj3)';
end

function [cliques nstates] = mkExamplePairwiseLPIModelCliquesAndStates()

    cliques = cell(14, 1);

    % if p(a) then if q(a) then 0.600000000 else 0.400000000 else if q(a) then 0.550000000 else 0.450000000
    % if p(a) and q(a) then 0.200000000 else 0.800000000
    % 1 = p(a)
    % 2 = q(a)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 0.360000;
    T(sub2ind(size(T), 1,2)) = 0.440000;
    T(sub2ind(size(T), 2,1)) = 0.320000;
    T(sub2ind(size(T), 2,2)) = 0.120000;
    cliques{1} = tabularFactorCreate(T, [1 2]);

    % if p(b) then if q(b) then 0.600000000 else 0.400000000 else if q(b) then 0.550000000 else 0.450000000
    % if p(b) and q(b) then 0.200000000 else 0.800000000
    % 3 = p(b)
    % 4 = q(b)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 0.360000;
    T(sub2ind(size(T), 1,2)) = 0.440000;
    T(sub2ind(size(T), 2,1)) = 0.320000;
    T(sub2ind(size(T), 2,2)) = 0.120000;
    cliques{2} = tabularFactorCreate(T, [3 4]);

    % if p(obj3) then if q(obj3) then 0.600000000 else 0.400000000 else if q(obj3) then 0.550000000 else 0.450000000
    % if p(obj3) and q(obj3) then 0.200000000 else 0.800000000
    % 5 = p(obj3)
    % 6 = q(obj3)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 0.360000;
    T(sub2ind(size(T), 1,2)) = 0.440000;
    T(sub2ind(size(T), 2,1)) = 0.320000;
    T(sub2ind(size(T), 2,2)) = 0.120000;
    cliques{3} = tabularFactorCreate(T, [5 6]);

    % if q(a) then 0.100000000 else 0.900000000
    % 2 = q(a)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.900000;
    T(sub2ind(size(T), 2)) = 0.100000;
    cliques{4} = tabularFactorCreate(T, [2]);

    % if q(b) then 0.100000000 else 0.900000000
    % 4 = q(b)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.900000;
    T(sub2ind(size(T), 2)) = 0.100000;
    cliques{5} = tabularFactorCreate(T, [4]);

    % if q(obj3) then 0.100000000 else 0.900000000
    % 6 = q(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.900000;
    T(sub2ind(size(T), 2)) = 0.100000;
    cliques{6} = tabularFactorCreate(T, [6]);

    % if r(a) then 0.300000000 else 0.700000000
    % 7 = r(a)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{7} = tabularFactorCreate(T, [7]);

    % if r(b) then 0.300000000 else 0.700000000
    % 8 = r(b)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{8} = tabularFactorCreate(T, [8]);

    % if r(obj3) then 0.300000000 else 0.700000000
    % 9 = r(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 0.700000;
    T(sub2ind(size(T), 2)) = 0.300000;
    cliques{9} = tabularFactorCreate(T, [9]);

    % if r(a) and p(a) then 0.250000000 else 0.750000000
    % 1 = p(a)
    % 7 = r(a)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 0.750000;
    T(sub2ind(size(T), 1,2)) = 0.750000;
    T(sub2ind(size(T), 2,1)) = 0.750000;
    T(sub2ind(size(T), 2,2)) = 0.250000;
    cliques{10} = tabularFactorCreate(T, [1 7]);

    % if r(b) then if p(b) then 0.650000000 else 0.350000000 else if p(b) then 0.640000000 else 0.360000000
    % 3 = p(b)
    % 8 = r(b)
    T = reshape(zeros(1, 4), 2, 2);
    T(sub2ind(size(T), 1,1)) = 0.360000;
    T(sub2ind(size(T), 1,2)) = 0.350000;
    T(sub2ind(size(T), 2,1)) = 0.640000;
    T(sub2ind(size(T), 2,2)) = 0.650000;
    cliques{11} = tabularFactorCreate(T, [3 8]);

    % p(a)
    % 1 = p(a)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{12} = tabularFactorCreate(T, [1]);

    % p(b)
    % 3 = p(b)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{13} = tabularFactorCreate(T, [3]);

    % p(obj3)
    % 5 = p(obj3)
    T = zeros(2, 1);
    T(sub2ind(size(T), 1)) = 1.000000;
    T(sub2ind(size(T), 2)) = 1.000000;
    cliques{14} = tabularFactorCreate(T, [5]);

    % Note: currently all LPI random variables only have 2 states (i.e. {false, true}).
    nstates = 2*ones(9, 1);

end
