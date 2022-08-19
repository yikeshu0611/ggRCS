RCS <- function(..., knot=NULL, reference='optimal', by=NULL){
    if (is.character(reference)){
        if (!all(reference %in% c('optimal','median'))){
            stop('reference must be numeric or optimal(defult) or median')
        }
    }
    fit2 <- list(...)
    modelname2 <- do::get_names(...)


    old <- options()

    options(datadist = '.zhishi.RCS')

    ######################## no by ###################################
    if (is.null(by)){
        x <- lapply(1:length(fit2), function(i){
            fiti <- fit2[[i]]
            di <- do::model.data(fiti)
            rcsx <- rcsx(fiti)

            # Knot
            optk <- getKnot(fiti)
            if (!is.null(knot)){
                if (all(knot == 'optimal')){
                    optk <- optimalKnot(fiti,title = modelname2[i])
                    optk <- optk[optk$min == '***',1][1]
                    fiti <- updateKnot(fiti,optk)
                }else if (is.numeric(knot)){
                    knot <- rep(knot,length(fit2))[1:length(fit2)][i]
                    fiti <- updateKnot(fiti,optk)
                }
            }




            Nonlinear <- anova(fiti)[" Nonlinear",'P']

            cat('\n')
            cat(crayon::red('########## model: ',modelname2[i],' ##########'))

            dd <- rms::datadist(di)
            .GlobalEnv[['.zhishi.RCS']] <- dd

            # reference
            if (length(reference)==1) refi <- reference
            if (length(reference) > 1) refi <- reference[i]
            if (is.na(refi)){
                if (do::cnOS()) stop('reference\u4E2A\u6570\u548C\u6A21\u578B\u4E2A\u6570\u4E0D\u4E00\u81F4')
                if (do::cnOS()) stop('The number of references is inconsistent with the number of models')
            }
            if (refi == 'median'){
                referenceString <- paste0(rcsx,' reference: median (',dd$limits['Adjust to',rcsx],')')
                ref <- dd$limits['Adjust to',rcsx][1]

            }else if (refi == 'optimal'){
                ref <- getReference(fiti)[1,]
                setReference(ref)
                ref <- ref[1,rcsx]
                referenceString <- paste0(rcsx,' reference: optimal (',ref,')')
            }else if(is.numeric(refi)){
                ref <- refi
                .GlobalEnv$.zhishi.RCS$limits['Adjust to',rcsx] <- ref
                referenceString <- paste0(rcsx,' reference: you specify (',ref,')')
            }
            cat('\n     Knot: ',optk,' (NL-Pvalue:',round(Nonlinear,4),')')
            cat('\n    ',referenceString)

            fiti <- update(fiti)
            x <- eval(parse(text=sprintf(fmt = 'Predict(fiti,%s,fun=exp,ref.zero=TRUE)',rcsx)))
            x$modelName <- modelname2[i]
            x$rcsName <- rcsx
            x$method <- deparse(fiti$call[[1]])
            x$Ref <- ref
            class(x) <- 'data.frame'
            x
        }) |> do.call(what = plyr::rbind.fill)

        rm(.zhishi.RCS,envir = .GlobalEnv)
        if (is.null(old$datadist)) options(datadist = NULL)
        options(old)

        mn <- ifelse(length(unique(x$modelName))==1,1,2)

        class(x) <- c(paste0('m',mn),'data.frame')
        attr(x,'rcsx') <- rcsx(fit2[[1]])

        return(x)

    ########################  by ###################################
    }else{
        by2 <- paste0(by,collapse = ' - ')
        ev <- new.env()
        ev$level <- NULL
        x <- lapply(1:length(fit2),function(i) {
            fiti <- fit2[[i]]
            rcsx <- rcsx(fiti)
            di <- do::model.data(fiti)

            if (length(by)==1) bycat = di[,by] else bycat = do::paste0_columns(di[,by],';;;')
            byu <- unique(bycat)
            byu <- as.character(byu[!is.na(byu)])
            if (!is.null(levels(bycat))) byu <- levels(bycat)[levels(bycat) %in% byu]
            if (is.null(ev$level)){
                ev$level <- byu
            }else{
                ev$level <- unique(c(ev$level,byu))
            }

            cat('\n')
            cat(crayon::red('########## model: ',modelname2[i],' ##########'))

            # subset
            r <- lapply(1:length(byu), function(ki){

                k <- byu[ki]

                dsub <- di[bycat == k,]
                dd <- suppressWarnings(rms::datadist(dsub))
                .GlobalEnv$.zhishi.RCS <- dd
                ei <- eval(parse(text=sprintf("update(fiti,formula. = .~. - %s, data=dsub)",by2)))


                # Knot
                optk <- getKnot(ei)
                if (!is.null(knot)){
                    if (all(knot == 'optimal')){
                        optk <- optimalKnot(ei,title = paste(modelname2[i],':',rcsx,'==',k),data = dsub)
                        optk <- optk[optk$min == '***',1][1]
                        ei <- updateKnot(ei,optk,data = dsub)
                    }else if (is.numeric(knot)){
                        if (length(knot)==1){
                            opt <- knot
                        }else{
                            if (length(knot) != length(fit2)*length(byu)){
                                stop('Number of Models: ',length(fit2)*length(byu),'\n','While number of knot: ',length(knot))
                            }
                        }
                        optk <- knot[length(byu)*(i-1)+ki]
                        ei <- updateKnot(ei,optk,data = dsub)
                    }
                }
                Nonlinear <- anova(ei)[" Nonlinear",'P']

                # reference
                if (length(reference)==1) refi <- reference
                if (length(reference) > 1) refi <- reference[(i-1)*ki+ki]
                if (is.na(refi)){
                    if (do::cnOS()) stop('reference\u4E2A\u6570\u548C\u6A21\u578B\u4E2A\u6570\u4E0D\u4E00\u81F4')
                    if (do::cnOS()) stop('The number of references is inconsistent with the number of models')
                }
                if ( refi== 'median'){
                    ref <- dd$limits['Adjust to',rcsx]
                    referenceString <- paste0(rcsx,'reference: median (',ref,')')

                }else if (refi == 'optimal'){
                    ref <- getReference(ei,dsub)[1,]
                    setReference(ref)
                    ref <- ref[1,rcsx]
                    referenceString <- paste0(rcsx,'reference: optimal (',ref,')')
                }else if(is.numeric(refi)){
                    ref <- refi
                    .GlobalEnv$.zhishi.RCS$limits['Adjust to',rcsx] <- ref
                    referenceString <- paste0(rcsx,'reference: you specify (',ref,')')
                }



                cat(crayon::blue('\n','group: ',by,'==',k))
                cat('\n     ','Knot:',optk,' (NL-Pvalue:',round(Nonlinear,4),')')
                cat('\n     ',referenceString)
                cat('\n     ',paste0(deparse(ei$call$formula),collapse = '') |> do::Replace(' {2,}',' '))
                ei <- update(ei)
                x <- eval(parse(text=sprintf(fmt = 'Predict(ei,%s,fun=exp,ref.zero=TRUE)',rcsx)))

                x$modelName <- modelname2[i]
                x$rcsName <- rcsx
                x$method <- deparse(ei$call[[1]])
                class(x) <- 'data.frame'
                x <- cbind(x,do::col_split(k,';;;',colnames = by))
                x$Ref <- ref
                x
            }) |> do.call(what = rbind)
        }) |> do.call(what = rbind)

        rm(.zhishi.RCS,envir = .GlobalEnv)
        if (is.null(old$datadist)) options(datadist = NULL)
        options(old)

        for (i in 1:length(by)) {
            x[,by[i]] <- factor(x[,by[i]],levels = unique(do::col_split(ev$level,';;;')[,i]))
        }
        mn <- ifelse(length(unique(x$modelName))==1,1,2)


        class(x) <- c(sprintf('m%sby',mn),'data.frame')
        attr(x,'by') <- by
        attr(x,'rcsx') <- rcsx(fit2[[1]])
        x
    }
}

