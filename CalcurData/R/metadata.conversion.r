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
	df=cbind(df,date=sapply(strsplit(df[,"datetime"]," "),function(x)x[1]))
	
	df=cbind(sitedate=df[,"date"],code=df[,"location"],ResightID=df[,"brand"],ltag=df[,"leftstatus"],
			rtag=df[,"rightstatus"],repstatus=df[,"repstatus"],behavior=df[,"behavior"],photo=df[,"photo"],
			brandcond=df[,"condition"],observer=rep("JDH",nrow(df)),woundcode=df[,"woundcode"],imagenum=df[,"image"],comments=df[,"comments"])
	if(append)
		write.table(df,con_out,row.names=FALSE,col.names=FALSE,sep="\t")
	else
		write.table(df,con_out,row.names=FALSE,sep="\t")
	close(con_out)
	invisible()
}
#' Convert bites resight file from ACDSee format to data entry template format 
#' 
#' @param inp name of input file containing metadata from ACDsee
#' @param out name of output file which is formatted for data copying to data entry template
#' @param append if TRUE, append to the output file
#' @return no return value
#' @export resights 
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


