require(pkgsearch)
package="sf"
cph<-cran_package_history(package)
versions<-cph$Version
version_package<-versions[length(versions)]
cph[cph$Version==version_package,"dependencies"]
cp=cran_package(package,version_package)
base_uri<-"https://cran.r-project.org/src/contrib/"

eapi<-7
# modifying package name
package_uri <- gsub("[\\.\\-]","_",package)

# version for the url
manage_version<-function(version_package)
{
  version_package_sep<-as.numeric(unlist(strsplit(version_package,"[\\.\\-]",perl=T)))
  version_separators<-regmatches(version_package,gregexec("[\\.\\-]",version_package,perl = T))[[1]][1,]
  stopifnot(length(version_separators)==(length(version_package_sep)-1))
  if(all(version_separators=="."))
  {version_variable="${PV}"} else {
    whichhyphen<-which(version_separators=="-")
    whichpoint<-which(version_separators==".")
    ls_vercut<-vector(mode="list",length=length(whichhyphen)+1)
    pointLsVercut1<-(whichpoint[whichpoint<whichhyphen[1]])
    if(length(pointLsVercut1)==0){ls_vercut[[1]]<-1}else{ls_vercut[[1]]<-c(1,max(pointLsVercut1)+1)}
    for(i in 1:length(whichhyphen))
    {
      isLast<-!any(whichhyphen>whichhyphen[i])
      if(isLast)
      {
        ls_vercut[[i+1]]<-unique(c(whichhyphen[i]+1,length(version_package_sep)))
      }else{
        wp<-whichpoint[whichpoint > whichhyphen[i] & whichpoint < whichhyphen[i+1]]
        if(length(wp)==0)
        {ls_vercut[[i+1]]<-whichhyphen[i]+1}else{
          ls_vercut[[i+1]]<-range(min(wp)-1,max(wp)+1)
          }
      }
    }
  version_variable<-paste(sapply(ls_vercut,function(x){
   if(length(x)==1)
     return(paste0("$(ver_cut ",x,")"))
   return(paste0("$(ver_cut ",x[1],"-",x[2],")"))  
  }),collapse="-")
  }
  return(c(version=version_package, version_formatted=paste(version_package_sep,sep=".",collapse = "."), version_variable=version_variable))
}

# Get url for the tar.gz source of a R package
get_uri_cran<-function(package,version,last=T,urlCran="https://cran.r-project.org")
{
  baseURI<-ifelse(last,paste(urlCran,"src/contrib",sep="/"),paste(urlCran,"src/contrib/Archive",package,sep="/"))
  file<-paste0(package,"_",version,".tar.gz")
  return(paste(baseURI,file,sep="/"))
}

# check whether the tar.gz source of a R package exists
try(z<-url(get_uri_cran("MASS","7.3-0",last=F),open="r"))
try(z<-url(get_uri_cran("MASS_noexist","7.3-0",last=F),open="r"))

# We need to figure the way to manage the license from that
A<-cran_events(limit=10000)
B<-unlist(sapply(A,function(x)x$package$License))
B2<-strsplit(B,"( \\+ )|( \\| )",perl=T)
sort(table(unlist(B2)))

# find github
extract_github<-function(url,notFound=NULL)
{
  REGEX<-"^https://github\\.com/([^/]+)/([^/]+)(/.*)?$"
  if(!grepl(REGEX,url,perl=T)){return(notFound)}
  return(sub(REGEX,'\\1/\\2',url,perl=T))
}
get_webpage_from_cran<-function(packageHistory,version=NA,
                                types=list(
                                  `github page`="^https://[^/]\\.github.io",
                                  `cran page`="^https://cran.r-project.org/web/packages/",
                                  `github repo`="^https://github\\.com/([^/]+)/([^/]+)(/.*)?$"
                                ))
{
  if(!is.na(version) & ! version %in% packageHistory$Version)
  {stop("Unrecognized version ",version)}
  sp_urls<-lapply(lapply(strsplit(packageHistory$URL,", "),na.omit),unique)
  infoURL<-data.frame(date=rep(packageHistory$`Date/Publication`,sapply(sp_urls,length)),
                      sameVersion=rep(version==packageHistory$Version,sapply(sp_urls,length)),
                      url=unlist(sp_urls)
  )
                      
             
}
get_github_from_cran<-function(packageHistory,result=c("last","all"))
{
  result<-match.arg(result)
  in_url<-lapply(strsplit(packageHistory$URL,", "),sapply,extract_github,notFound=NA)
  in_br<-lapply(strsplit(packageHistory$BugReports,", "),sapply,extract_github,notFound=NA)
  in_url<-lapply(in_url,na.omit)
  in_br<-lapply(in_br,na.omit)
  if(max(sapply(in_url,length))==0 & max(sapply(in_br,length)==0)){return(NA)}
  tbFound<-rbind(
  data.frame(date=rep(packageHistory$`Date/Publication`,sapply(in_url,length)),
             field="URL",
             github=unlist(in_url)
             ),
  data.frame(date=rep(packageHistory$`Date/Publication`,sapply(in_br,length)),
             field="BugReports",
             github=unlist(in_br)
             )
  )
  tbFound<-tbFound[order(tbFound$date,tbFound$field=="URL",decreasing=T),]
  return(switch(result,
         last=tbFound[1,"github"],
         all=unique(tbFound$github)))
}


sapply(unlist(strsplit(cph$URL,", ")),extract_github)
sapply(cph$BugReports,extract_github)

cph$`Date/Publication`
strsplit(cph$URL,", ")
cph$BugReports

  t(sapply(cph$Version,manage_version))

inherit<-"R-packages"
description<-cp$Title
keywords<-"~amd64"
