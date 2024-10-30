
get_pd <- function(tree, species){

# get all species in matched cube

all_matched_sp<-unique(mcube[["orig_tiplabel"]])

# the following paragraph is redundant because getMRCA can also use tip labels
# directly

# get tip id's from tip labels

# all_tip_ids <- vector(mode="integer", length=length(all_matched_sp))
# for (i in seq_along(all_matched_sp)) {
#  x <- which(tree$tip.label == all_matched_sp[i])
#  all_tip_ids[i] <- x
# }

# find most recent common ancestor
MRCA <- getMRCA(tree, all_tip_ids)

# calculate PD metric
calculate_faithpd(tree, species)
}
