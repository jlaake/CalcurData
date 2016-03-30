#' Convert resight file from ACDSee format to data entry template format 
#' 
#' @param inp name of input file containing metadata from ACDsee
#' @param out name of output file which is formatted for data copying to data entry template
#' @param append if TRUE, append to the output file
#' @return no return value
#' @export resights 
#' @author Jeff Laake, Jeff Harris
resights=function(inp,out,append=FALSE)
{
	if(append)  
		con_out=file(out,open="at")
	else
		con_out=file(out,open="wt")
	xx=readLines(inp)
	xnam=c("Title","Headline","Description Writer","Address","City","Country","Creator","Job Title","Date/Time","Postal Code","State/Province","Web URL")
	ivec=sapply(xnam,function(x) regexpr(x,xx[4])[1])
	image=gsub(" ","",substr(xx[-(1:5)],1,ivec[1]-1))
	brand=gsub(" ","",(substr(xx[-(1:5)],ivec[1],ivec[2]-1)))
	location=gsub(" ","",substr(xx[-(1:5)],ivec[2],ivec[3]-1))
	condition=gsub(" ","",substr(xx[-(1:5)],ivec[3],ivec[4]-1))
	condition[condition=="BRANDCONDITION"&nchar(brand)<=4]=sapply(nchar(brand),function(x) paste(rep("0",x),collapse=""))[condition=="BRANDCONDITION"&nchar(brand)<=4]
	condition=gsub(" ","",condition)
	condition=gsub("'","",condition)
	condition=paste("'",condition,sep="")
	condition[condition=="'BRANDCONDITION"]=""
	repstatus=substr(xx[-(1:5)],ivec[4],ivec[5]-1)
	repstatus=gsub(" ","",repstatus)
	repstatus[repstatus=="REPSTATUS"]=""
	behavior=gsub(" ","",substr(xx[-(1:5)],ivec[5],ivec[6]-1))
	behavior[behavior=="BEHAVIOR"]=""
	photo=rep("Y",length(brand))
	leftstatus=gsub(" ","",substr(xx[-(1:5)],ivec[7],ivec[8]-1))
	rightstatus=gsub(" ","",substr(xx[-(1:5)],ivec[8],ivec[9]-1))
	datetime=substr(xx[-(1:5)],ivec[9],ivec[10]-1)
	comments=substr(xx[-(1:5)],ivec[10],ivec[11]-1)
	comments[substr(comments,1,8)=="COMMENTS"]=""
	woundcode=gsub(" ","",substr(xx[-(1:5)],ivec[11],ivec[12]-1))
	woundcode[woundcode=="WOUNDCODE"]=""
	sel=brand!="BRAND"
	df=cbind(image=image,brand=brand,location=location,condition=condition,repstatus=repstatus,behavior=behavior,photo=photo,leftstatus=leftstatus,rightstatus=rightstatus,datetime=datetime,comments=comments,woundcode=woundcode)
	df=df[sel,]
	df=cbind(df,date=sapply(strsplit(df[,"datetime"]," "),function(x)x[1]),time=sapply(strsplit(df[,"datetime"]," "),function(x)x[2]))
	
	df=cbind(sitedate=df[,"date"],code=df[,"location"],ResightID=df[,"brand"],ltag=df[,"leftstatus"],
			rtag=df[,"rightstatus"],repstatus=df[,"repstatus"],behavior=df[,"behavior"],photo=df[,"photo"],
			brandcond=df[,"condition"],observer=rep("JDH",nrow(df)),comments=df[,"comments"],woundcode=df[,"woundcode"],imagenum=df[,"image"],time=df[,"time"])
	cnt=sapply(df[,"ResightID"],function(x) length(grep(";",x)))
	if(all(cnt==0))
	{
		if(append)
			write.table(df,con_out,row.names=FALSE,col.names=FALSE,sep="\t")
		else
			write.table(df,con_out,row.names=FALSE,sep="\t")
	} else
	{
		xdf=NULL
		for(i in 1:nrow(df))
		{
			ids=strsplit(df[i,"ResightID"],";")[[1]]
			tempdf=df[rep(i,length(ids)),,drop=FALSE]
			tempdf[,"ResightID"]=ids
			code=strsplit(df[i,"code"],";")[[1]]
			if(length(code)!=length(ids) &length(code)!=1)stop("error in code value in line ",i)
			code[toupper(code)=="X"]=""
			tempdf[,"code"]=code
			ltag=strsplit(df[i,"ltag"],";")[[1]]
			if(length(ltag)==0)ltag="X"
			if(length(ltag)!=length(ids) &length(ltag)!=1)stop("error in ltag value in line ",i)
			ltag[toupper(ltag)=="X"]=""
			tempdf[,"ltag"]=ltag
			rtag=strsplit(df[i,"rtag"],";")[[1]]
			if(length(rtag)==0)rtag="X"
			if(length(rtag)!=length(ids) &length(rtag)!=1)stop("error in rtag value in line ",i)
			rtag[toupper(rtag)=="X"]=""
			tempdf[,"rtag"]=rtag
			repstatus=strsplit(df[i,"repstatus"],";")[[1]]
			if(length(repstatus)==0)repstatus="X"
			if(length(repstatus)!=length(ids) &length(repstatus)!=1)stop("error in repstatus value in line ",i)
			repstatus[toupper(repstatus)=="X"]=""
			tempdf[,"repstatus"]=repstatus
			behavior=strsplit(df[i,"behavior"],";")[[1]]
			if(length(behavior)==0)behavior="X"
			if(length(behavior)!=length(ids) &length(behavior)!=1)stop("error in behavior value in line ",i)
			behavior[toupper(behavior)=="X"]=""
			tempdf[,"behavior"]=behavior
			brandcond=strsplit(df[i,"brandcond"],";")[[1]]
			if(length(brandcond)==0)brandcond="X"
			if(length(brandcond)>1)
			   brandcond[2:length(brandcond)]=paste("'",brandcond[2:length(brandcond)],sep="")
			if(length(brandcond)!=length(ids) &length(brandcond)!=1)stop("error in brandcond value in line ",i)
			brandcond[toupper(brandcond)=="X"]=""
			tempdf[,"brandcond"]=brandcond
			comment=strsplit(df[i,"comments"],";")[[1]]
			if(length(comment)==0)comment="X"
			if(length(comment)!=length(ids) &length(comment)!=1)stop("error in comment value in line ",i)
			comment[str_trim(toupper(comment))%in%c("X","")]=""
			tempdf[,"comments"]=str_trim(comment)
			xdf=rbind(xdf,tempdf)
		}
		if(append)
			write.table(xdf,con_out,row.names=FALSE,col.names=FALSE,sep="\t")
		else
			write.table(xdf,con_out,row.names=FALSE,sep="\t")
	}
	close(con_out)
	invisible()
}
#' Convert bites resight file from ACDSee format to data entry template format 
#' 
#' @param inp name of input file containing metadata from ACDsee
#' @param out name of output file which is formatted for data copying to data entry template
#' @param append if TRUE, append to the output file
#' @return no return value
#' @export bites 
#' @author Jeff Laake, Jeff Harris
bites=function(inp,out,append=FALSE)
{
	setblank=function(x,value) 
	{
		x[x==value]=""
		return(x)
	}
	if(append)  
		con_out=file(out,open="at")
	else
		con_out=file(out,open="wt")
	xx=readLines(inp)
	xnam=c("Date/Time Original","Title","Headline","Description","Description Writer","IPTC Subject Code","Creator","Job Title","Address","City","Country","Email","IPTC Scene Code","Phone","Postal Code","State/Province","Web URL")
	ivec=sapply(xnam,function(x) regexpr(x,xx[4])[1])
	xnam=c("City","Intellectual Genre","Location","State/Province","Copyright Notice","Rights Usage Terms")  
	ivec=c(ivec,sapply(xnam,function(x) regexpr(x,substr(xx[4],ivec[length(ivec)],1000))[1])+ivec[length(ivec)]-1)
	image=gsub(" ","",substr(xx[-(1:5)],1,ivec[1]-1))
	datetime=substr(xx[-(1:5)],ivec[1],ivec[2]-1)
	animalID=gsub(" ","",(substr(xx[-(1:5)],ivec[2],ivec[3]-1)))
	ageclass=gsub(" ","",(substr(xx[-(1:5)],ivec[3],ivec[4]-1)))
	ageclass=setblank(ageclass,"AGECLASS")
	sex=gsub(" ","",(substr(xx[-(1:5)],ivec[4],ivec[5]-1)))
	sex=setblank(sex,"SEX")
	location=gsub(" ","",(substr(xx[-(1:5)],ivec[5],ivec[6]-1)))
	location[location=="LOCATION"]="SMI"
	area=gsub(" ","",(substr(xx[-(1:5)],ivec[6],ivec[7]-1)))
	area=setblank(area,"AREA")
	species=gsub(" ","",(substr(xx[-(1:5)],ivec[7],ivec[8]-1)))
	species[species=="SPECIES"]="Zc"
	side=gsub(" ","",(substr(xx[-(1:5)],ivec[8],ivec[9]-1)))
	side=setblank(side,"SIDE")
	sides_seen=gsub(" ","",(substr(xx[-(1:5)],ivec[9],ivec[10]-1)))
	sides_seen=setblank(sides_seen,"#SIDESSEEN")
	bite_location=gsub(" ","",(substr(xx[-(1:5)],ivec[10],ivec[11]-1)))
	bite_location=setblank(bite_location,"BITELOCATION")
	wound_class=gsub(" ","",(substr(xx[-(1:5)],ivec[11],ivec[12]-1)))
	wound_class=setblank(wound_class,"WOUNDCLASS")
	comments=substr(xx[-(1:5)],ivec[12],ivec[13]-1)
	comments[substr(comments,1,8)=="COMMENTS"]=""
	brand=gsub(" ","",(substr(xx[-(1:5)],ivec[13],ivec[14]-1)))
	brand=setblank(brand,"BRAND")
	shark_species=gsub(" ","",(substr(xx[-(1:5)],ivec[14],ivec[15]-1)))
	shark_species=setblank(shark_species,"SHARKSPECIES")
	age_of_bite=gsub(" ","",(substr(xx[-(1:5)],ivec[15],ivec[16]-1)))
	age_of_bite=setblank(age_of_bite,"AGEOFBITE")
	secondary_location=gsub(" ","",(substr(xx[-(1:5)],ivec[16],ivec[17]-1)))
	secondary_location=setblank(secondary_location,"SECONDARYLOCATION")
	observer=gsub(" ","",(substr(xx[-(1:5)],ivec[17],ivec[18]-1)))
	observer=setblank(observer,"OBSERVER")
	entire_lesion=gsub(" ","",(substr(xx[-(1:5)],ivec[18],ivec[19]-1)))
	entire_lesion=setblank(entire_lesion,"ENTIRELESION?")
	severity=gsub(" ","",(substr(xx[-(1:5)],ivec[19],ivec[20]-1)))
	severity=setblank(severity,"SEVERITY")
	pup=gsub(" ","",(substr(xx[-(1:5)],ivec[20],ivec[21]-1)))
	pup=setblank(pup,"PUP")
	numIDToothMarks=gsub(" ","",(substr(xx[-(1:5)],ivec[21],ivec[22]-1)))
	numIDToothMarks=setblank(numIDToothMarks,"#IDENTIFIABLETOOTHMARKS")
	numLesions=gsub(" ","",(substr(xx[-(1:5)],ivec[22],ivec[23]-1)))
	numLesions=setblank(numLesions,"#OFLESIONS")
	LesionSize=gsub(" ","",(substr(xx[-(1:5)],ivec[23],ivec[23]+20)))
	LesionSize=setblank(LesionSize,"LESIONSIZE")
	sel=animalID!="ANIMALID"
	df=cbind(image=image,animalID=animalID,ageclass=ageclass,sex=sex,location=location,area=area,species=species,
			side=side,sidees_seen=sides_seen,bite_location=bite_location,wound_class=wound_class,comments=comments,brand=brand,
			shark_species=shark_species,age_of_bite=age_of_bite,secondary_location=secondary_location,observer=observer,
			entire_lesion=entire_lesion,severity=severity,pup=pup,numIDToothMarks=numIDToothMarks,numLesions=numLesions,
			LesionSize=LesionSize)
	df=df[sel,]
	df=cbind(date=sapply(strsplit(datetime[sel]," "),function(x)x[1]),time=sapply(strsplit(datetime[sel]," "),function(x)x[2]),df)
	if(append)
		write.table(df,con_out,row.names=FALSE,col.names=FALSE,sep="\t")
	else
		write.table(df,con_out,row.names=FALSE,sep="\t")
	close(con_out)
	invisible()
}
#' Convert resight file from ACDSee format for Ej sightings to data entry template format 
#' 
#' @param inp name of input file containing metadata from ACDsee; if NULL can select from file browser
#' @param out name of output file which is formatted for data copying to data entry template
#' @param append if TRUE, append to the output file
#' @return no return value
#' @export ejresights 
#' @author Jeff Laake, Jeff Harris
ejresights=function(inp=NULL,out,append=FALSE)
{
	if(is.null(inp))inp=file.choose()
	library(stringr)
	if(append)  
		con_out=file(out,open="at")
	else
		con_out=file(out,open="wt")
	xx=readLines(inp)
	xnam=c("Filename","Title","Headline","Description","Address","City","Country","Creator","Email","Job Title","IPTC Scene Code","Phone","Postal Code","State/Province",
			"Web URL","Copyright Notice","Rights Usage Terms","Intellectual Genre","Location")   
	ivec=sapply(xnam,function(x) regexpr(x,xx[4])[1])
	image=gsub(" ","",substr(xx[-(1:5)],1,ivec[2]-1))
	brand=gsub(" ","",(substr(xx[-(1:5)],ivec[2],ivec[3]-1)))
	datetime=substr(xx[-(1:5)],ivec[3],ivec[4]-1)
	date=sapply(strsplit(datetime," "),function(x)x[1])
	datemat=do.call("rbind",strsplit(date,"/"))
	region=gsub(" ","",substr(xx[-(1:5)],ivec[5],ivec[6]-1))
	sitename=str_trim(substr(xx[-(1:5)],ivec[6],ivec[7]-1))
	condition=gsub(" ","",substr(xx[-(1:5)],ivec[7],ivec[8]-1))
	condition[condition=="QUALITY"&nchar(brand)<=4]=sapply(nchar(brand),function(x) paste(rep("0",x),collapse=""))[condition=="QUALITY"&nchar(brand)<=4]
	condition=gsub(" ","",condition)
	condition=gsub("'","",condition)
#	condition=paste("'",condition,sep="")
	condition[condition=="QUALITY"]=""
	primaryobs=gsub(" ","",substr(xx[-(1:5)],ivec[8],ivec[9]-1))
	leftstatus=gsub(" ","",substr(xx[-(1:5)],ivec[9],ivec[10]-1))
	rightstatus=gsub(" ","",substr(xx[-(1:5)],ivec[15],ivec[16]-1))
	secondaryobs=gsub(" ","",substr(xx[-(1:5)],ivec[10],ivec[11]-1))
	comments=str_trim(substr(xx[-(1:5)],ivec[11],ivec[12]-1))
	photo=rep("YES",length(brand))
	tagnumber=gsub(" ","",substr(xx[-(1:5)],ivec[12],ivec[13]-1))
	check=gsub(" ","",substr(xx[-(1:5)],ivec[13],ivec[14]-1))
	platform=gsub(" ","",substr(xx[-(1:5)],ivec[14],ivec[15]-1))
	age=gsub(" ","",substr(xx[-(1:5)],ivec[16],ivec[17]-1))
	sex=gsub(" ","",substr(xx[-(1:5)],ivec[17],ivec[18]-1))
	rbi=gsub(" ","",substr(xx[-(1:5)],ivec[18],ivec[19]-1))
	sel=brand!="Brand/TagID"
	bl=substr(brand,nchar(brand),nchar(brand))
	bl2=substr(brand,1,1)
	bl.last=suppressWarnings(is.na(as.numeric(bl)))
	bl.first=suppressWarnings(is.na(as.numeric(bl2)))
	BrandLetter=bl
	BrandLetter[bl.first]=bl2[bl.first]
	BrandNumber=substr(brand,1,nchar(brand)-1)
	BrandNumber[bl.first]=substr(brand[bl.first],2,nchar(brand[bl.first]))
	BrandNumber=suppressWarnings(as.numeric(BrandNumber))
	check[check=="Y"]="YES"
    sex[sex=="SEX"]=NA
	age[age=="AGE"]=NA
	rbi[rbi=="RIB"]=NA
	comments[substr(comments,1,8)=="COMMENTS"]=""
    secondaryobs[secondaryobs=="SECONDARYOBS"]=""
    sitename[sitename=="CARMANAH"]="CARMANAH POINT"
	sitename[sitename=="BODELTEH"]="BODELTEH ISLAND"
	sitename[sitename=="E. BODELTEH"]="E. BODELTEH ISLAND"
	sitename[sitename=="W. BODELTEH"]="W. BODELTEH ISLAND"
	df=data.frame(BrandLetter=BrandLetter,BrandNumber=BrandNumber,Brand=brand,BrandLocation="L",BrandQuality=condition,Check=check,TagNumber=tagnumber,LTag=leftstatus,RTag=rightstatus,
			TagColor="W",Month=as.numeric(datemat[,2]),Day=as.numeric(datemat[,3]),Year=as.numeric(datemat[,1]),Time="",Sex=sex,Age=age,
			Region=region,SiteName=sitename,SubSite="",RBI=rbi,Obs=primaryobs,Obs2=secondaryobs,Platform=platform,photo=photo,Comments=comments,image=image,DateEntered=format(Sys.time()),Radio="",Patch="",stringsAsFactors=FALSE)	
	sel=brand!="Brand/TagID"
	df=df[sel,]
	# get rid of duplicates and combine image numbers
	images=tapply(df$image,df$Brand,function(x) paste(x,collapse=","))
	dfmerge=data.frame(Brand=names(images),image=as.vector(images))
	df=merge(subset(df,select=names(df)[names(df)!="image"]),dfmerge,by="Brand")
	dupsall=duplicated(df[,!names(df)=="image"])
	df=df[!dupsall,]
	dups=duplicated(subset(df,select=c("Brand","Month","Day","Year")))
	if(any(dups))
	{
		cat("\nFollowing records are duplicates for Brand-Date but information varies. Please collapse.")
		print(df[dups,])
		stop("\nStopping without saving.\n")
	}
	if(append)
		write.table(df,con_out,row.names=FALSE,col.names=FALSE,sep="\t")
	else
		write.table(df,con_out,row.names=FALSE,sep="\t")
	close(con_out)
	invisible()
}
#' Validates Ej resight data  
#' 
#' @param filename name of file output from ejresights function; if NULL can select with file browser
#' @return no return value
#' @export ejcheck
#' @author Jeff Laake, Jeff Harris

ejcheck=function(filename=NULL)
{
	if(is.null(filename))filename=file.choose()    
	con=file(filename,open="rt")
	df=read.table(con,sep="\t",header=TRUE,stringsAsFactors=FALSE,colClasses="character")
#	colC=sapply(df,mode)
#    colC[colC=="logical"]="character"
#	con1=file(filename,open="rt")
#	df=read.table(con1,sep="\t",header=TRUE,stringsAsFactors=FALSE,colClasses=colC)
	tblSex=getCalcurData("Ej",tbl="tblSex")
	sex_values=as.character(tblSex$Sex)
	tblSitenames=getCalcurData("Ej",tbl="tblSitenames")
	site_values=as.character(tblSitenames$sitename)
	tblRegions=getCalcurData("Ej",tbl="tblRegions")
	region_values=as.character(tblRegions$Region)
	tblRBI=getCalcurData("Ej",tbl="tblRBI")
	rbi_values=as.character(tblRBI$RBI)
	tblObs=getCalcurData("Ej",tbl="tblObs")
	obs_values=c("",as.character(tblObs$Obs))
	tblTag=getCalcurData("Ej",tbl="tblLRTag")	
	tag_values=as.character(tblTag$LRTag)
	tblcheck=getCalcurData("Ej",tbl="tblBrandCheck")	
	check_values=as.character(tblcheck$BrandCheck)
	tblage=getCalcurData("Ej",tbl="tblAge")	
	age_values=as.character(tblage$Age)
	tblBrandLetter=getCalcurData("Ej",tbl="tblBrandLetter")	
	brandletter_values=as.character(tblBrandLetter$BrandLetter)
	tblplatform=getCalcurData("Ej",tbl="tblPlatform")	
	platform_values=as.character(tblplatform$Platform)
	tblbrand= getCalcurData("Ej",tbl="tblBrands")  
	brand_values=as.character(tblbrand$Brand)
	# loop through records and test
	for( i in 1:nrow(df))
	{
		cat("\nRecord ",i,":")
		ok=TRUE
		test=function(field,values)
		{
			if(!df[i,field]%in%values)
			{
				cat("\nBad value for ",field," = ",as.character(df[i,field]))
				return(FALSE)
			} else
				return(TRUE)
		}
		ok=test("Sex",sex_values)
		ok=ok&test("Platform",platform_values)
		ok=ok&test("SiteName",site_values)
		ok=ok&test("Region",region_values)
		ok=ok&test("RBI",rbi_values)
		ok=ok&test("Obs",obs_values)
		ok=ok&test("Obs2",obs_values)
		ok=ok&test("LTag",tag_values)
		ok=ok&test("RTag",tag_values)
		ok=ok&test("Check",check_values)
		ok=ok&test("Age",age_values)
		ok=ok&test("BrandLetter",brandletter_values)
		ok=ok&test("Brand",brand_values)
		ok=ok&test("Month",as.character(1:12))
		ok=ok&test("Day",as.character(1:31))
		if(!(as.numeric(as.character(df[i,"Year"]))>2000))
		{
			cat("\nBad value for Year = ",as.character(df[i,"Year"]))
			ok=FALSE
		}
		if(ok) cat("ok\n")
	}
	invisible()
}


