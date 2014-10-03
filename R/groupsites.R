p4s <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

limit.vds <- function(sites,direction,freeway){
    keep.idx <- sites@data$freeway_dir == direction &
        sites@data$freeway_id == freeway
    if(sum(keep.idx)==0) return (NULL)
    sites[keep.idx,]
}

limit.wim <- function(sites,direction,freeway){
    keep.idx <- sites@data$direction == direction &
        sites@data$freeway_id == freeway
    if(sum(keep.idx)==0) return (NULL)
    sites[keep.idx,]
}

fixup.priority.sites <- function(pw=NULL,pv=NULL,direction,freeway){
    allowed.wim <- NULL
    allowed.vds <- NULL
    if(!is.null(pw) ){
        ## could be WIM, could be VDS
        if(length(pw@data$direction)==0){
            ## not WIM
            allowed.vds <- limit.vds(pw
                                    ,direction=direction
                                    ,freeway=freeway)
        }else{
            ## wim
            allowed.wim <- limit.wim(pw
                                     ,direction=direction
                                     ,freeway=freeway)
        }
    }
    if(!is.null(pv) ){
        ## could be WIM, could be VDS and be careful to rbind, as
        ## prior loop might have already created allowed.wim,
        ## allowed.vds
        if(length(pv@data$direction)==0){
            ## not WIM
            allow <- limit.vds(pv
                              ,direction=direction
                              ,freeway=freeway)
            if(!is.null(allowed.vds)){
                allowed.vds <- rbind(allowed.vds,allow)
            }else{
                allowed.vds <- allow
            }
            rm(allow)
        }else{
            ## wim
            allow <- limit.wim(pv
                               ,direction=direction
                               ,freeway=freeway)
            if(!is.null(allowed.wim)){
                allowed.wim <- rbind(allowed.wim,allow)
            }else{
                allowed.wim <- allow
            }
            rm(allow)
        }
    }
    return (list("allowed.wim"=allowed.wim
                ,"allowed.vds"=allowed.vds))
}

match.sites <- function(target.sites,other.sites,distance,already.covered){

    using.wim <- length(target.sites@data$direction)>0

    if(dim(target.sites)[1]==0)
        return (list('other.sites'=other.sites,'already.covered'=already.covered))

    target.other.km <- spDists(target.sites
                              ,other.sites
                              ,longlat=TRUE)
    close.idx <- target.other.km < distance

    while( sum(close.idx[,!already.covered]) > 0  ){
        if(sum(!already.covered)==1){ ## edge case, else rowsums fails
            convert.site <- which.min(already.covered)
        }else{
            ranking <- rowSums(close.idx[,!already.covered])
            convert.site <- which.max(ranking)
        }
        new.sites.idx <- close.idx[convert.site,]

        ## The ORing above just blocked out the already paired site
        ## from being considered as a "convert.site".  The following
        ## assignment also gets rid of them from the new
        ## convert.site's targets

        ## drop ones already covered by another site
        new.sites.idx[already.covered] <- FALSE

        if(using.wim){
            other.sites@data$group[new.sites.idx] <-
                paste(c('wim',
                        target.sites@data[convert.site,
                                           c('site_no',
                                             'direction')])
                     ,collapse='.')
        }else{ ## VDS site, not WIM site
            other.sites@data$group[new.sites.idx] <-
                target.sites@data[convert.site,'id']
        }
        already.covered <- already.covered | close.idx[convert.site,]
    }
    return (list('other.sites'=other.sites,'already.covered'=already.covered))
}

#' groupsites
#'
#' Group together sites to choose the best sites as CVDS
#'
#' @param dfv the VDS sites, as a dataframe.  Ideally these should have just one freeway and direction.  Use plyr to get this to happen
#' @param distance the max distance from the candidate CVDS site
#' @param wim.priority.sites,vds.priority.sites  SpatialPointsDataFrame objects containing the WIM sites, and/or the preferred VDS sites to use.  The VDS sites do not have to overlap with the sites in dfv.  Neither set is required.  Also, both will be trimmed to make sure that the same freeway and direction are used as for the vds sites themselves
#' @return a dataframe, mostly the same as the input dfv, but with a new column called "group" added that contains the wim or vds site that is used to group each of the sites.
#' @examples
#' \donttest{
#' newdf <- groupsites(vdf,distance=10,wim.priority.sites=all.the.wims)
#' }
#' @export
groupsites <- function(dfv,distance=16
                      ,wim.priority.sites=NULL
                      ,vds.priority.sites=NULL
                       ){

    ## cheating here, but too lazy to do otherwise
    freeway_dir <- dfv$freeway_dir[1]
    freeway_id <-  dfv$freeway_id[1]

    ## temp diagnostic to catch hanging loops
    print(paste('processing: '
               ,freeway_dir
               ,freeway_id
               ,'with'
               ,dim(dfv)[1]
               ,'points'))

    l <- fixup.priority.sites(direction=freeway_dir
                             ,freeway=freeway_id
                             ,pv=vds.priority.sites
                             ,pw=wim.priority.sites)

    allowed.vds=l[["allowed.vds"]]
    allowed.wim=l[["allowed.wim"]]

    ## re-create spatial points data frame objects
    sp::coordinates(dfv) <- c('coords.x1','coords.x2')
    sp::proj4string(dfv) <- p4s

    ## catch possible positional errors in vds, wim
    ## ducktyping here

    covered.idx <- rep(FALSE,dim(dfv)[1])

    if(!is.null(allowed.wim) && dim(allowed.wim)[1]>0){
        l <- match.sites(allowed.wim,dfv,distance,covered.idx)
        dfv <- l[['other.sites']]
        covered.idx <- l[['already.covered']]

    }



    wim.covered.idx <-  covered.idx

    if(!is.null(allowed.vds) && dim(allowed.vds)[1]>0){
        l <- match.sites(allowed.vds,dfv,distance,covered.idx)
        dfv <- l[['other.sites']]
        covered.idx <- l[['already.covered']]

    }

    ## and then the rest, if needed


    l <- match.sites(dfv,dfv,distance,covered.idx)
    dfv <- l[['other.sites']]
    covered.idx <- l[['already.covered']]

    vds.covered.idx <- covered.idx
    vds.covered.idx[wim.covered.idx] <- FALSE
    dfv@data$group <- as.factor(dfv@data$group)

    ## return something useful
    ##
    ## not sure if I want to return wim.covered.idx and
    ## vds.covered.idx.  now they seem redundant
    return (as.data.frame(dfv))
}
