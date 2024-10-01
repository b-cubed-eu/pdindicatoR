using Phylo
using Plots
using Diversity

# Define custom equality for LinkBranch
Base.:(==)(a::LinkBranch, b::LinkBranch) = a.name == b.name

# Define custom equality for Node
Base.:(==)(a::LinkNode, b::LinkNode) = a.name == b.name

function object_id(obj)
    return objectid(obj)
end

function linkbranch_equal(branch1::LinkBranch, branch2::LinkBranch)
    return branch1.name == branch2.name
end

# Load the tree from a Newick file
function load_newick(fileName)
    
    filePath = joinpath(@__DIR__, "..", "data", fileName)
    tree = open(parsenewick, filePath)
    return tree
end


# function to remove common elements in the paths
function remove_common_branches(vectors)
    # Convert vectors to a Set for easier comparison
    sets = [Set(subvec) for subvec in vectors]
    
    # Find the common elements in all sets
    common_elements = intersect(sets...)
    
    # Return the common elements as a vector
    return collect(common_elements)
end

# Function to find the path from a ancestor to a node
function path_to_ancestor(tree, node, ancestor)
    path = []
    current = node
    while current !== ancestor
        push!(path, current)
        current = getparent(tree, current)
        if getinbound(tree,current) === missing  # If the node has no parent, we've reached the root
            break
        end
    end
    push!(path, ancestor)
    reverse!(path)
    return path
end

#= Function to find the lowest common ancestor (LCA) of a set of leaves
function lowest_common_ancestor(tree, leaves)
    # Get paths to root for each leaf
    paths = [path_to_ancestor(tree, leaf, nothing) for leaf in leaves]
    
    # Find the set of all ancestors for each path
    ancestor_sets = [Set(path) for path in paths]
    
    # Find the common ancestors
    common_ancestors = reduce(intersect, ancestor_sets)
    
    # Sort common ancestors by depth (assuming depth can be determined somehow, e.g., by path length)
    common_ancestors_sorted = sort(collect(common_ancestors), by=node -> length(path_to_ancestor(tree, node, nothing)), rev=true)
    
    # Return the deepest common ancestor
    return isempty(common_ancestors_sorted) ? nothing : first(common_ancestors_sorted)
end
=#

# Function to find the branches that span the minimum path between the leaves
function branches_span_leaves(tree, leaves)
    lca = mrca(tree, leaves)
    n = length(leaves)
    branches = fill([], n)
    for leaf in leaves
        j = 1
        path = path_to_ancestor(tree, leaf, lca)
        for i in 1:length(path)-1
            edge = getinbound(tree, path[i])
            #edge = (path[i], path[i+1])
            push!(branches[j], edge)
        end
        j += 1
    end

    return branches
end

# function to sum over the branch lengths
function sum_branch_lengths(tree, branches)
    bl = 0
    for branch in branches
        if haslength(tree, branch)
            bl += getlength(tree, branch)
        end
    end
    return bl
end

#= FOR TESTING PURPOSES ONLY

# exemplar use
fileName = "Acacia.nwk"
tree = load_newick(fileName)


# Plot the tree
plot(tree, treetype = :fan, showtips=false)

# Test distance between two species
test = distance(tree, "Acacia_aciphylla", "Acacia_hammondii")

# Get all the species
test2 = getleafnames(tree)
branches = collect(getbranches(tree))

# Define the leaf names of interest

# Random set of names
# leaf_names = ["Acacia_ashbyae", "Acacia_woodmaniorum", "Acacia_aphylla"]

# Small subtree to test (6 branches)
leaf_names = ["Acacia_anasilla", "Acacia_adoxa", "Acacia_perryi"]

leaves = [getnode(tree, name) for name in leaf_names]

# Get the spanning branches for the specified leaves
spanning_branches = branches_span_leaves(tree, leaves)

stripped_branches = remove_common_branches(spanning_branches)

pd = sum_branch_lengths(tree, stripped_branches)

println("Phylogenetic diversity: ", pd)

=#