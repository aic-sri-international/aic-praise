function mrf = factorGraphToMrf(fg, varargin)
%% Convert a factor graph to a Markov random field
% Note from mrfCreate.m:
%{
Currently the interface only accepts potentials
on nodes and edges, however support for general
clique potentials may be added later.
%}
% so we will create pairwise Markov random fields in
% a similar manner to factorGraphToMrf2.
% See mrfCreate for additional optional args.
%%

% 

fg = factorGraphMakePairwise(fg); % does nothing if fg is already pairwise
G = constructGraphFromFactors(fg.factors);

%% nodePots
nodeFacNdx = fg.nodeFacNdx;
pots = fg.factors(nodeFacNdx);
npots = numel(pots);
nodePots = cell(npots, 1);
for i=1:npots
    nodePots{i} = pots{i};
end

%% edgePots
edgeFacNdx = fg.edgeFacNdx;
pots = fg.factors(edgeFacNdx);
npots = numel(pots);
edgePots = cell(npots, 1);
% Note: can map directly and ignore the linear ordering of the edges in 
% the graph as we are using tabular factors and not numeric matrices 
% (see mrfCreate.m).
for i=1:npots
    edgePots{i} = pots{i};
end

%% create mrf
mrf = mrfCreate(G, 'nodePots', nodePots, 'edgePots', edgePots, ... 
                   varargin{:});
    
end
