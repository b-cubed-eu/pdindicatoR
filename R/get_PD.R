

# get all species in matched cube
# use not unique_name but tree_name!

all_matched_sp<-unique(mcube[["unique_name"]])

# get tip id's from tip labels

tip_ids <- vector(mode="integer", length=length(all_matched_sp))
for (i in seq_along(all_matched_sp)) {
  x <- which(tree$tip.label == all_matched_sp[i])
  tip_ids[i] <- x
}

# find most recent common ancestor
MRCA <- getMRCA(tree, tip_ids)

# calculate PD metric
calculate_FaithPD(tree, tip_ids)
