res <- load(file='./wim.df.R')
res <- load(file='./sample.data.1.R')
res <- load(file='./sample.data.2.R')
res <- load(file='./sample.data.30.R')
res <- load(file='./sample.data.405.R')

priority.vds.sites <- as.data.frame(sample.data.405)[c(1,80,120),]
sp::coordinates(priority.vds.sites) <- c('coords.x1','coords.x2')
sp::proj4string(priority.vds.sites) <- sp::proj4string(sample.data.405)

test_that('fixup priority sites works as expected',{


    l <- fixup.priority.sites(direction=sample.data.405@data$freeway_dir[1]
                             ,freeway=sample.data.405@data$freeway_id[1]
                             ,pv=priority.vds.sites
                             ,pw=wim.df)

    expect_that(priority.vds.sites,equals(l[['allowed.vds']]))
    expect_that(dim(l[['allowed.wim']]),equals(c(2,9)))

    ## and the reverse is okay too
    l <- fixup.priority.sites(direction=sample.data.405@data$freeway_dir[1]
                             ,freeway=sample.data.405@data$freeway_id[1]
                             ,pw=priority.vds.sites
                             ,pv=wim.df)
    expect_that(priority.vds.sites,equals(l[['allowed.vds']]))
    expect_that(dim(l[['allowed.wim']]),equals(c(2,9)))

    ## only VDS is okay
    l <- fixup.priority.sites(direction=sample.data.405@data$freeway_dir[1]
                             ,freeway=sample.data.405@data$freeway_id[1]
                             ,pv=priority.vds.sites
                             ##,pw=wim.df
                              )
    expect_that(priority.vds.sites,equals(l[['allowed.vds']]))
    expect_null(l[['allowed.wim']],'expect that wim result is null')

    ## only WIM is okay
    l <- fixup.priority.sites(direction=sample.data.405@data$freeway_dir[1]
                             ,freeway=sample.data.405@data$freeway_id[1]
                             ##,pv=priority.vds.sites
                             ,pw=wim.df
                              )
    expect_that(dim(l[['allowed.wim']]),equals(c(2,9)))
    expect_null(l[['allowed.vds']],'expect that vds result is null')

})

test_that('matching sites code works as expected',{


    l <- fixup.priority.sites(direction=sample.data.405@data$freeway_dir[1]
                             ,freeway=sample.data.405@data$freeway_id[1]
                             ,pv=priority.vds.sites
                             ,pw=wim.df)
    expect_that(priority.vds.sites,equals(l[['allowed.vds']]))
    expect_that(dim(l[['allowed.wim']]),equals(c(2,9)))

    allowed.vds <- l[['allowed.vds']]
    allowed.wim <- l[['allowed.wim']]

    ## various ways to call match.sites

    empty.idx <-  rep(FALSE,dim(sample.data.405)[1])

    l <- match.sites(allowed.wim,sample.data.405,16,empty.idx)
    dfv <- l[['other.sites']]
    wim.match.idx <- l[['already.covered']]
    expect_that(levels(as.factor(l[['other.sites']]@data$group))
               ,equals(c("wim.112.N", "wim.13.N")))
    expect_that(sum(is.na(l[['other.sites']]@data$group)),equals(80))

    l <- match.sites(allowed.vds,sample.data.405,16,empty.idx)
    expect_that(levels(as.factor(l[['other.sites']]@data$group))
               ,equals(c("717758",  "767367",  "1211066")))
    expect_that(sum(is.na(l[['other.sites']]@data$group)),equals(56))

    ## now with the wim.match.idx, get a different result
    l <- match.sites(allowed.vds,dfv,16,wim.match.idx)
    expect_that(sort(levels(as.factor(l[['other.sites']]@data$group)))
               ,equals(sort(c("717758",  "1211066","wim.112.N", "wim.13.N"))))
    expect_that(sum(is.na(l[['other.sites']]@data$group)),equals(12))

    current.matched <- l[['already.covered']]
    dfv <- l[['other.sites']]
    ## test all to all
    l <- match.sites(dfv,dfv,16,current.matched)

    expect_that(sort(levels(as.factor(l[['other.sites']]@data$group)))
               ,equals(sort(c("717758","1211066"
                        ,"wim.112.N","wim.13.N"
                        ,"772455","1218071")))
                )

    expect_that(sum(is.na(l[['other.sites']]@data$group)),equals(0))

    ## now with highway 1, which barfs too

    l <- fixup.priority.sites(direction=sample.data.1@data$freeway_dir[1]
                             ,freeway=sample.data.1@data$freeway_id[1]
                             ,pw=wim.df)

    allowed.vds <- l[['allowed.vds']]
    allowed.wim <- l[['allowed.wim']]

    expect_null(allowed.vds)
    expect_null(allowed.wim)


    ## step through as if groupsites was running
    empty.idx <-  rep(FALSE,dim(sample.data.1)[1])
    dfv <- sample.data.1
    expect_error(match.sites(allowed.wim,dfv,16,empty.idx))

    expect_error( match.sites(allowed.vds,dfv,16,empty.idx))

    ## test all to all
    l <- match.sites(dfv,dfv,16,empty.idx)

    expect_that(sort(levels(as.factor(l[['other.sites']]@data$group)))
               ,equals(sort(c("405573","500011063")))
                )

    expect_that(sum(is.na(l[['other.sites']]@data$group)),equals(0))

})

print('group sites tests')

test_that('groupsites will assign values to all locations',{
    df <- groupsites(as.data.frame(sample.data.405),16,wim.df)

    expect_that(dim(df),equals(c(154,18)))

    expect_that(is.element(c("wim.112.N","wim.13.N"),df$group)
                ,equals(c(TRUE,TRUE)))
    expect_is(df,"data.frame")
    expect_that(sum(is.na(df$group)),equals(0))
})


test_that('groupsites will assign values to all locations take 2',{
    df <- groupsites(as.data.frame(sample.data.1),16,wim.df)
    expect_that(dim(df),equals(c(18,18)))
    expect_that(sum(is.na(df$group)),equals(0))
    ## expect_that(df$group) is not NA...
})

test_that('just one location will work okay',{
    df <- groupsites(as.data.frame(sample.data.30),16,wim.df)
    expect_that(dim(df),equals(c(1,18)))
})

test_that('groupsites works without a wim data set',{

    df <- groupsites(as.data.frame(sample.data.405),16,wim.df)
    expect_that(sort(unique(df$group)),
                equals(as.factor(
                    c(1211066,1218071,717760,772455,'wim.112.N','wim.13.N')
                )))

    df <- groupsites(as.data.frame(sample.data.405),16)
    expect_that(sort(unique(df$group)),
                equals(as.factor(
                  c(717799,718251,759427,771808,1214461,1218071)
                )))

    df.2.nowim <- groupsitesr::groupsites(as.data.frame(sample.data.2),16)
    expect_that(dim(df),equals(c(14,18)))
    expect_is(df,"data.frame")
    expect_that(sum(is.na(df$group)),equals(0))

    df.2.wim <- groupsites(as.data.frame(sample.data.2),16,wim.df)
    expect_that(dim(df),equals(c(14,18)))
    expect_is(df,"data.frame")
    expect_that(sum(is.na(df$group)),equals(0))

})

test_that('specific VDS sites can be chosen',{


    df <- groupsites(as.data.frame(sample.data.405),16,priority.vds.sites)

    expect_that(is.element(priority.vds.sites$id,df$group)
                ,equals(c(TRUE,TRUE,TRUE)))

})

test_that('One can mix vds and wim sites in the priority set',{


    df <- groupsites(as.data.frame(sample.data.405),16,priority.vds.sites)

    expect_that(is.element(priority.vds.sites$id,df$group)
                ,equals(c(TRUE,TRUE,TRUE)))

})
