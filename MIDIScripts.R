setwd("E:/2017/Documents/R")
load('MIDIStuff.RData')

ExtractTracks = function(dataIn,exClude=c(),n=16,opLegacy=F){
  if(opLegacy){
    output = list();
    # data.frame('Time'=c(),'Track'=c(),'Note'=c(),'Length'=c(),'Velocity'=c());
    j = 1;
    maxT = 0;
    minP = 128;
    maxP = 0;
    for(i in 1:n){
      if(!(i %in% exClude)){
        tmp = dataIn[dataIn$track==i,];
        if(nrow(tmp)!=0){
          output$TMP = tmp[c('time','note','velocity','length')];
          if(max(tmp$time + tmp$length)>maxT) maxT = max(tmp$time+tmp$length);
          if(max(tmp$note)>maxP) maxP = max(tmp$note);
          if(min(tmp$note)<minP) minP = min(tmp$note);
          tmpNameList = names(output);
          tmpNameList[tmpNameList == 'TMP'] = as.character(j);
          names(output) = tmpNameList;
          j = j+1;
        }
      }
    }
    output$META = data.frame('maxT'=maxT,'minP'=minP,'maxP'=maxP);
  }
  else{
    dataTMP = dataIn;
    dataIn = dataIn$midi;
    output = list();
    # data.frame('Time'=c(),'Track'=c(),'Note'=c(),'Length'=c(),'Velocity'=c());
    # j = 1;
    maxT = 0;
    minP = 128;
    maxP = 0;
    j = 0;
    tmpInst = list('0'=0,'1'=0,'2'=0,'3'=0,'4'=0,'5'=0,'6'=0,'7'=0,
                       '8'=0,'9'=0,'10'=0,'11'=0,'12'=0,'13'=0,'14'=0,'15'=0);
    for(i in 0:n){
      if(!(i %in% exClude)){
        tmp = dataIn[dataIn$channel==i,];
        if(nrow(tmp)!=0){
          if(i==9){
            thisName = '9';
          }
          else{
            thisName = j;
            j = j + 1;
            if(j==9) j = 10;
          }
          output$TMP = tmp[c('time','note','velocity','length')];
          if(max(tmp$time + tmp$length)>maxT) maxT = max(tmp$time+tmp$length);
          if(max(tmp$note)>maxP) maxP = max(tmp$note);
          if(min(tmp$note)<minP) minP = min(tmp$note);
          tmpNameList = names(output);
          tmpNameList[tmpNameList == 'TMP'] = thisName;
          names(output) = tmpNameList;
          if(i==9){
            tmpInst[[10]] = StatMode(dataTMP$meta$instruments[[10]])
          }
          else{
            tmpInst[[j]] = StatMode(dataTMP$meta$instruments[[i+1]]);
          }
          
          # j = j+1;
        }
      }
    }
    # tmpInst = lapply(dataTMP$meta$instruments,StatMode);
    # names(tmpInst) = as.character(0:15);
    # tmpInst = lapply(tmpInst,function(x){if(is.na(x)) x = 0 else x});

    output$META = list('maxT'=maxT,'minP'=minP,'maxP'=maxP,
                        'tick'=dataTMP$meta$tick,
                        'tsig'=c(dataTMP$meta$tsig,dataTMP$meta$ppq,dataTMP$meta$epq),
                        'ksig'=dataTMP$meta$ksig,
                        'bpm'=dataTMP$meta$bpm,
                        'inst'=tmpInst,
                        'resolution'=NA,
                        'matmode'=NA,
                        'sustainmode'=NA);
  }
  return(output);
}

PlotTracks = function(dataIn,n=NULL,xlimIn = NULL,ylimIn=NULL,opMode=1,colOff=1,lineWid = 3,opPiano = T,opBars = T){
  if(is.null(xlimIn)){
    theLimx = c(0,dataIn$META$maxT);
  }
  else{
    theLimx = xlimIn;
  }
  if(is.null(ylimIn)){
    theLimy = c(dataIn$META$minP,dataIn$META$maxP);
  }
  else{
    theLimy = ylimIn;
  }
  
  if(opBars){
    plot(NULL,NULL,'p',ylim=theLimy,xlim=theLimx,yaxt = 'n',xaxt = 'n');
    
    barY1 = vector(length = floor(dataIn$META$maxT/(4*dataIn$META$tick))+1)+theLimy[2]+10;
    barY2 = vector(length = floor(dataIn$META$maxT/(dataIn$META$tick))+1)+theLimy[2]+10;
    barX1 = (0:floor(dataIn$META$maxT/(4*dataIn$META$tick)))*(4*dataIn$META$tick);
    barX2 = (0:floor(dataIn$META$maxT/(dataIn$META$tick)))*(dataIn$META$tick);
    lines(barX2,barY2,'h',col='gray');
    lines(barX1,barY1,'h',col='black');
    axis(side = 1, at = barX1, labels = 0:floor(dataIn$META$maxT/(4*dataIn$META$tick)));
  }
  else{
    plot(NULL,NULL,'p',ylim=theLimy,xlim=theLimx,yaxt = 'n');
  }
  axis(side = 2, at = seq(from=0,to=131,by=12), labels = paste0(paste0('(C',as.character(-1:9),sep=') '),(0:10)*12,sep=''));
  
  if(is.null(n)){
    toPlot = seq(1:(length(dataIn)-1));
  }
  else{
    toPlot = n;
  }
  
  for(i in 1:length(toPlot)){
    if(!opMode){
      lines(dataIn[[toPlot[i]]]$time,dataIn[[toPlot[i]]]$note, 'p',pch=21,  bg=i+colOff);
    }
    else{
      for(j in 1:nrow(dataIn[[toPlot[i]]])){
        lines(c(dataIn[[toPlot[i]]]$time[j],dataIn[[toPlot[i]]]$time[j]+dataIn[[toPlot[i]]]$length[j]),c(dataIn[[toPlot[i]]]$note[j],dataIn[[toPlot[i]]]$note[j]),'l',lwd = lineWid,col=i+colOff);
      }
    }
  }
  
  if(opPiano){
    DrawPiano(theLimy[1]-10,theLimy[2]+10,theLimx[1]);
  }
}

DrawPiano = function(a=0,b=131,drawStart=0){
  theNotes = c(0,1,0,1,0,0,1,0,1,0,1,0);
  for(i in a:b){
    tmpI = theNotes[(i%%12)+1];
    if(tmpI){
      rect(drawStart-15000,i-0.5,drawStart,i+0.5,col='black',border='black');
    }
    else{
      rect(drawStart-15000,i-0.5,drawStart,i+0.5,col='white',border='black');
    }
  }
}

NoteDistribution = function(songIn,music=T,drums=T,modulo=T){
  outPut1 = list();
  outPut2 = list();
  tmpLength = length(songIn)-1;
  
  if(modulo){
    modulo = 12;
  }
  else{
    modulo = 132;
  }
  
  outNotes3 = vector(mode='numeric',length=modulo);
  outNotes4 = vector(mode='numeric',length=modulo);
  for(i in 1:tmpLength){
    thisTrack = songIn[[i]];
    isDrum = names(songIn[i])=='9';
    if((!isDrum)*music + isDrum*drums){
      outNotes1 = vector(mode='numeric',length=modulo);
      outNotes2 = vector(mode='numeric',length=modulo);
        for(j in 1:nrow(thisTrack)){
          outNotes1[thisTrack[j,]$note%%modulo+1] = outNotes1[thisTrack[j,]$note%%modulo+1]+1
          outNotes2[thisTrack[j,]$note%%modulo+1] = outNotes2[thisTrack[j,]$note%%modulo+1]+thisTrack[j,]$length;
        }
      outNotes3 = outNotes3 + outNotes1;
      outNotes4 = outNotes4 + outNotes2;
      outNotes1 = outNotes1/max(outNotes1);
      outNotes2 = outNotes2/max(outNotes2);
      outPut1[[i]] = outNotes1;
      outPut2[[i]] = outNotes2;
    }
    else{
      outPut1[[i]] = vector(length = modulo)*NaN;
      outPut2[[i]] = vector(length = modulo)*NaN;
    }
  }
  
  outPut1 = as.data.frame.list(outPut1,col.names= 1:tmpLength);
  outPut2 = as.data.frame.list(outPut2,col.names = 1:tmpLength);
  outPut3 = data.frame('count'=outNotes3/max(outNotes3),'length'=outNotes4/max(outNotes4));
  outPut = list('count'=outPut1,'length'=outPut2,'totals'=outPut3);
  return(outPut);
}

TrackToMatrix = function(trackIn,nB,dSmp = 1,trackMeta,sustainMode=0,maxVel=127){
  tmpLen = nrow(trackIn);
  out = matrix(0,ncol = 132*(1+max(sustainMode-1,0)),nrow = nB*dSmp);
  
  for(i in 1:tmpLen){
    thisEvent = trackIn[i,];
    # out[(TickToBar(thisEvent$time,trackMeta)*dSmp+1):(TickToBar(thisEvent$time+thisEvent$length,trackMeta)*dSmp),thisEvent$note] = thisEvent$velocity;
    if(sustainMode==2){
      out[(TickToBar(thisEvent$time,trackMeta)*dSmp+1):(TickToBar(thisEvent$time+thisEvent$length,trackMeta)*dSmp),(thisEvent$note+1+132)] = maxVel;
      out[(TickToBar(thisEvent$time,trackMeta)*dSmp+1),(thisEvent$note+1+132)] = 0;
      out[(TickToBar(thisEvent$time,trackMeta)*dSmp+1),(thisEvent$note+1)] = thisEvent$velocity;
    }
    else if(sustainMode==1){
      out[(TickToBar(thisEvent$time,trackMeta)*dSmp+1):(TickToBar(thisEvent$time+thisEvent$length,trackMeta)*dSmp),(thisEvent$note+1)] = -thisEvent$velocity;
      out[(TickToBar(thisEvent$time,trackMeta)*dSmp+1),(thisEvent$note+1)] = thisEvent$velocity;
    }
    else{
      out[(TickToBar(thisEvent$time,trackMeta)*dSmp+1):(TickToBar(thisEvent$time+thisEvent$length,trackMeta)*dSmp),(thisEvent$note+1)] = thisEvent$velocity;
    }
  }
  
  if(max(sustainMode-1,0)){
    colnames(out) = c(0:131,paste(0:131,'s',sep=''));
  }
  else{
    colnames(out) = 0:131;
  }
  
  rownames(out) = (0:(nrow(out)-1))/dSmp;
  return(out);
}

TrackConvolve = function(songIn,printT = T){
  tmpLen = length(songIn)-1;
  tmpDist = NoteDistribution(songIn);
  listCon = list();
  
  for(i in 1:tmpLen){
    toCon = (1:132)*(tmpDist$length[,i]>0);
    toCon = toCon[toCon!=0];
    thisTrack = TrackToMatrix(songIn,i);
    tmpCon = vector('numeric',nrow(thisTrack));
    for(j in 1:length(toCon)){
      tmpCon = tmpCon + convolve(thisTrack[,toCon[j]],thisTrack[,toCon[j]],type = 'circular');
    }
    listCon[[i]] = tmpCon/max(tmpCon);
    if(printT) cat(sprintf("%s \t %i \n",as.character(Sys.time()),i));
  }
  
  return(listCon)
}

ConvEnvelope = function(convIn,binSz = 2500){
  tmpLen = length(convIn);
  out = list();
  
  for(i in 1:tmpLen){
    thisTrack = convIn[[i]];
    trkLen = length(thisTrack);
    trkBin = trkLen/binSz;
    binBrk = round(seq(0,trkLen,length.out = trkBin)+1);
    trkMax = c();
    trkPts = c();
    for(j in 1:(length(binBrk)-1)){
      tmpRange = thisTrack[binBrk[j]:(binBrk[j+1]-1)]
      trkMax[j] = max(tmpRange);
      tmpPt = (1:binSz)[tmpRange==max(tmpRange)] + binBrk[j]-1;
      trkPts[j] = tmpPt[1];
    }
    #currentMax = thisTrack[1];
    #currentPos = 1
    #for(j in 1:trkLen){
      #tmpRange = thisTrack[max(c(j-round(binSz/2),1)):min(c(j+round(binSz/2),trkLen))];
      #tmpMax = max(tmpRange);
      #if(currentMax!=tmpMax){
        #trkMax = c(trkMax,currentMax);
        #trkPts = c(trkPts,currentPos);
        #currentMax = tmpMax;
        #currentPos = j + round(binSz/2);
      #}
    #}
    #trkMax = c(trkMax,currentMax);
    #trkPts = c(trkPts,currentPos);
    trkPts[j+1] = trkLen;
    trkMax[j+1] = thisTrack[trkLen];
    out[[i]] = approx(trkPts,trkMax,1:trkLen);
  }
  
  return(out);
}

MaxEnvelope = function(envIn){
  tmpLen = length(envIn);
  trkLen = length(envIn[[1]]$y);
  tmp = vector('numeric',trkLen);
  
  tmp[1] = NA;
  tmp[trkLen] = NA;
  
  for(i in 2:(trkLen-1)){
    tmpMax = envIn[[1]]$y[i];
    trkSelect = 1;
    for(j in 2:tmpLen){
      if(envIn[[j]]$y[i]>tmpMax){
        tmpMax = envIn[[j]]$y[i];
        trkSelect = j;
      }
    }
    tmp[i] = trkSelect;
  }
  
  out = vector('numeric',tmpLen);
  for(i in 1:tmpLen){
    out[i] = sum(tmp[2:(trkLen-1)]==i)/(trkLen-2);
  }
  
  return(out);
}

ExportMIDI = function(tracksIn,nameOut='hRmoniseOutput',optWrite = T,optPrint = F){
  # function to export tracks as a format-1 MIDI. MIDI should be in the same format as is output from ReadMIDI. optPrint dumps to console for debug.
  
  tick = tracksIn$META$tick;
  bpm = tracksIn$META$bpm[1];
  tSig = tracksIn$META$tsig;
  kSig = tracksIn$META$ksig;
  instName = tracksIn$META$inst;
  
  out = StringToHex('MThd')$out;
  out = c(out,as.raw(c(0,0,0,6,0,1)));
  nTrk = length(tracksIn);
  
  actualNTrk = 0;
  for(i in 1:(nTrk-1)){
    if(nrow(tracksIn[[i]])>0){
      actualNTrk = actualNTrk + 1;
    }
  }

  out = c(out,IntToHex(actualNTrk+1,2),IntToHex(tick,2));
  
  toAdd = c();
  toAdd = c(toAdd,as.raw(c(0x00,0xff,0x58)));
  toAdd = c(toAdd,as.raw(0x04),IntToHex(tSig[1]),IntToHex(log2(tSig[2])),IntToHex(tSig[3]),IntToHex(tSig[4]));
  
  toAdd = c(toAdd,as.raw(c(0x00,0xff,0x59)));
  tmpSign = sign(kSig[1]);
  tmpSign[tmpSign==0] = 1;
  toAdd = c(toAdd,as.raw(c(0x02,abs(kSig[1])+(tmpSign*-0.5+0.5)*16,kSig[2])));
  
  toAdd = c(toAdd,as.raw(c(0x00,0xff,0x51)));
  toAdd = c(toAdd,as.raw(0x03),IntToHex(round(60000000/bpm),3));
  
  toAdd = c(toAdd,as.raw(c(0x00,0xff,0x2f,0x00)));
  out = c(out,StringToHex('MTrk')$out,IntToHex(length(toAdd),4),toAdd);
  
  for(i in 1:(nTrk-1)){
    thisTrack = tracksIn[[i]];
    # if(i==7){
    #   Sys.sleep(0.1);
    # }
    if(nrow(thisTrack)!=0){
      thisChannel = as.numeric(names(tracksIn[i]));
      thisInst = instName[as.numeric(names(instName))==thisChannel][[1]];
      toAdd = c(as.raw(0x00),IntToHex(192+thisChannel),IntToHex(thisInst),
                as.raw(0x00),IntToHex(176+thisChannel),IntToHex(7),IntToHex(127));  # channel volume
      tmpTrk = data.frame('Time'=c(),'On'=c(),'Track'=c(),'Note'=c(),'Velocity'=c());
      for(j in 1:nrow(thisTrack)){
        tmpTrk = rbind(tmpTrk,data.frame('Time'=c(thisTrack[j,]$time,thisTrack[j,]$time+thisTrack[j,]$length),
                                         'On'=c(1,0),'Track'=c(thisChannel,thisChannel),'Note'=c(thisTrack[j,]$note,thisTrack[j,]$note),
                                         'Velocity'=c(thisTrack[j,]$velocity,0)));
      }
      tmpTrk = tmpTrk[order(tmpTrk$Time),];
      tmpTrk$Time = tmpTrk$Time - c(0,tmpTrk$Time[1:(nrow(tmpTrk)-1)]);
      for(j in 1:nrow(tmpTrk)){
        if(j==1){
          toAdd = c(toAdd,IntToVL(tmpTrk[j,]$Time),as.raw(0x80+tmpTrk[j,]$On*16+tmpTrk[j,]$Track),as.raw(c(tmpTrk[j,]$Note,as.raw(tmpTrk[j,]$Velocity))));
        }
        else{
          toAdd = c(toAdd,IntToVL(tmpTrk[j,]$Time),as.raw(c(tmpTrk[j,]$Note,as.raw(tmpTrk[j,]$Velocity))));
        }
      }
      toAdd = c(toAdd,IntToVL(tmpTrk[j,]$Time),as.raw(c(0xff,0x2f,0x00)));
      out = c(out,StringToHex('MTrk')$out,IntToHex(length(toAdd),4),toAdd);
    }
  }
  
  if(optWrite) writeBin(out,paste(nameOut,'.mid',sep = ''));
  if(optPrint) return(out);
}

StringToHex = function(str){
  # function to encode a string as hex, as would be used in MIDI standard
  
  tmpStr = strsplit(str,"")[[1]]
  tmpLen = length(tmpStr);
  
  out = c();
  for(i in 1:tmpLen){
    out = c(out,charToRaw(tmpStr[i]))
  }
  
  out = list('out'=out,'len'=tmpLen);
  return(out);
}

IntToHex = function(n,pad=0){
  # function to convert an integer to hex
  
  nByte = floor(log(n,256));
  out = c();
  
  if(nByte>0){
    for(i in nByte:1){
      tmp = floor(n/256^i);
      n = n - (256^i)*tmp;
      out = c(out,as.raw(tmp));
    }
  }
  out = c(out,as.raw(n));
  
  if(pad){
    while(length(out)<pad){
      out = c(as.raw(0),out);
    }
  }
  return(out);
}

IntToVL = function(n,byteLim = 4){
  # function to convert an integer to MIDI standard variable-length hex. Note general MIDI standard requires max number of bytes be 4
  
  nByte = floor(log(n,128));
  out = c();
  
  if(nByte>0){
    for(i in nByte:1){
      tmp = floor(n/128^i);
      n = n - (128^i)*tmp;
      out = c(out,as.raw(tmp+0x80));
    }
  }
  out = c(out,as.raw(n));

  if(length(out)>byteLim) stop('Input outside representable range (change byte limit)');
  return(out);
}

ReadMidi = function(fileIn){
  # function to read MIDI tracks from file. tuneR package dependency. Returns a list of tracks containing timing, note, velocity, notelength;
  # metadata stored in META
  
  con = file(description = fileIn, open = "rb");
  on.exit(close(con));
  
  MThd = readChar(con, 4);
  if (MThd != "MThd") 
    stop("No Header Chunk in this Midi (?)");
  MThd_length = readBin(con, integer(0), n = 1, size = 4, endian = "big");
  if (MThd_length != 6) 
    stop("Unexpected Header Chunk size");
  MThd_format = readBin(con, integer(0), n = 1, size = 2, endian = "big");
  if (!(MThd_format %in% 0:2)) 
    stop("Unexpected Mide file format");
  nTracks = readBin(con, integer(0), n = 1, size = 2, endian = "big");
  MThd_division = readBin(con, integer(0), n = 1, size = 2, signed = TRUE, endian = "big");
  if (MThd_division < 0) {
    stop("Midi representation of timing: Frames per second / ticks per frame not yet implemented, please ask the author");
  }
  
  allTracks = list();
  for(track in 1:nTracks){
    MTrk = readChar(con, 4);
    if (MTrk != "MTrk") 
      stop("No Track Chunk in this Midi");
    MTrk_length = readBin(con, integer(0), n = 1, size = 4, endian = "big");
    MeventList = list();
    bytes = 0;
    i = 0;
    while (bytes < MTrk_length) {
      i = i + 1;
      MeventList[[i]] = ReadMTrkEvent(con, if (i > 1) 
        MeventList[[i - 1]][["EventChannel"]]
        else NA);
      bytes = bytes + MeventList[[i]][["bytes"]];
      # if((bytes>117)&&(bytes<130)){ # debugging loop
      #   print('pause');
      # }
      # if((bytes>117)&&(bytes<130)){ # debugging loop
      #   print('pause');
      # }
    }
    if (MeventList[[i]][["type"]] != "2f") 
      stop("No end of track event after track length bytes");
    thisTrack = do.call("rbind", lapply(MeventList, function(x) as.data.frame(x, stringsAsFactors = FALSE)));
    thisTrack = thisTrack[, -(ncol(thisTrack) - (0:1))];
    thisTrack[, 1] = cumsum(thisTrack[, 1]);
    names(thisTrack)[1] = "time";
    thisTrack$track = track;
    allTracks[[track]] = thisTrack;
  }
  allTracks = do.call("rbind", allTracks);
  allTracks$event[allTracks$event == "Note On" & allTracks$parameter2 == 0] = "Note Off"
  allTracks$event = factor(allTracks$event, levels = c("Note Off", 
                                                        "Note On", "Note Aftertouch", "Controller", "Program Change", 
                                                        "Channel Aftertouch", "Pitch Bend", "Meta", "System", 
                                                        "Sequence Number", "Text Event", "Copyright Notice", 
                                                        "Sequence/Track Name", "Instrument Name", "Lyric", "Marker", 
                                                        "Cue Point", "Program Name", "Device Name", "MIDI Channel Prefix", 
                                                        "MIDI Port", "End of Track", "Set Tempo", "SMPTE Offset", 
                                                        "Time Signature", "Key Signature", "Sequencer Specific"));
  #return(allTracks);
  x = allTracks;
  
  tmpTime = strsplit(x[x$event=='Time Signature',"parameterMetaSystem"],', ')[[1]];
  tmpTSig = as.numeric(strsplit(tmpTime[1],'/')[[1]]);
  tmpClocks = as.numeric(strsplit(tmpTime[2],' clocks/tick')[[1]]);
  tmp8pQ = as.numeric(strsplit(tmpTime[3],' 1/32 notes / 24 clocks')[[1]]);
  
  tmpKey = strsplit(x[x$event=='Key Signature',"parameterMetaSystem"],' ');
  if(length(tmpKey)){
    tmpKey = tmpKey[[1]];
    if(tmpKey[2]=='major'){
      tmpMM = 0;
      }
    else{
      tmpMM = 1;
    }
    if(tmpMM){
      tmpKSig = (-7:7)[(c("Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B (H)", "F#", "C#", "G#", "D#", "A#")==tmpKey[1])];
    }
    else{
      tmpKSig = (-7:7)[(c("Cb", "Gb", "Db", "Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B (H)", "F#", "C#")==tmpKey[1])];
    }
    tmpKSig[2] = tmpMM;
  }
  else{
    tmpKSig = c(0,0);
  }
  
  instList = list();
  for(i in 0:15){
    instList[[i+1]] = x[(x$event=='Program Change')&(x$channel==i),"parameter1"];
  }

  tmpTempo = x[x$event=='Set Tempo',"parameterMetaSystem"];
  
  x = x[x$event %in% c("Note On", "Note Off"), c("time", "event", "channel", "parameter1", "parameter2", "track")];
  if (!nrow(x)) {
    return(data.frame(time = numeric(0), length = numeric(0), 
                      track = integer(0), channel = numeric(0), note = integer(0), 
                      notename = factor(NULL, levels = tuneR:::notenames(-69:62)),
                      velocity = integer(0)));
  }
  x = split(x, x$parameter1);
  # for(O in 1:length(x)){
  # i = x[[O]];
  # i = i[order(i$channel,i$time),];
  # j = 1;
  # itemp = data.frame(time=c(),event=c(),channel=c(),parameter1=c(),parameter2=c(),track=c(),stringsAsFactors = F);
  # k = 0;
  # t1 = c();
  # t2 = c();
  # while(j+k <= nrow(i)){
  #   if(j%%2) {
  #     while(((j+k)<=nrow(i))&&(i[j+k,]$event != "Note On")){
  #       k = k+1;
  #     }
  #     if((j+k)<=nrow(i)) t1 = c(t1,i[j+k,]$time);
  #   }
  #   else{
  #     while(((j+k)<=nrow(i))&&(i[j+k,]$event != "Note Off")){
  #       k = k+1;
  #     }
  #     if((j+k)<=nrow(i)) t2 = c(t2,i[j+k,]$time);
  #   }
  #   j = j+1;
  #   itemp = rbind(itemp,i[j+k,]);
  # }
  # # i = i[(i$time%in%t1)&(i$event=='Note On'),];
  # i = itemp;
  # i$length = t2-t1;
  # i;
  # }
  x = lapply(x, function(i) {
    i = i[order(i$channel,i$time),];
    j = 1;
    k = 0;
    t1 = c();
    t2 = c();
    itemp = data.frame(time=c(),event=c(),channel=c(),parameter1=c(),parameter2=c(),track=c());
    while(j+k <= nrow(i)){
      if(j%%2) {
        while(((j+k)<=nrow(i))&&(i[j+k,]$event != "Note On")){
          k = k+1;
        }
        if((j+k)<=nrow(i)){
          t1 = c(t1,i[j+k,]$time);
          itemp = rbind(itemp,i[j+k,]);
        }
      }
      else{
        while(((j+k)<=nrow(i))&&(i[j+k,]$event != "Note Off")){
          k = k+1;
        }
        if((j+k)<=nrow(i)) t2 = c(t2,i[j+k,]$time);
      }
      j = j+1;
    }
    # i = i[(i$time%in%t1)&(i$event=='Note On'),];
    i = itemp;
    i$length = t2-t1;
    i;
    #Time = matrix(i$time, byrow = TRUE, ncol = 2)
    #i = i[i$event == "Note On", ]
    #i$length = Time[, 2] - Time[, 1]
    #i
    });
  x = do.call("rbind", x);
  x$parameter1 = as.integer(x$parameter1);
  x$notename = factor(tuneR:::notenames(x$parameter1 - 69), levels = tuneR:::notenames(-69:62));
  x = x[order(x$track, x$time), !(names(x) %in% "event")];
  names(x) = c("time", "channel", "note", "velocity", "track", "length", "notename");
  rownames(x) = NULL;
  x = x[, c("time", "length", "track", "channel", "note", "notename", "velocity")];
  # return(x);
  x = list('midi'=x,'meta'=list('tick'=MThd_division,'bpm'=60000000/as.numeric(tmpTempo),'tsig'=tmpTSig,'ppq'=tmpClocks,'epq'=tmp8pQ,'ksig'=tmpKSig,'instruments'=instList));
  return(ExtractTracks(x));

}

ReadMTrkEvent = function(con, lastEventChannel = NA){
  # ReadMTrkEvent from tuneR package modified to remove bugs and handle edge cases better
  
  DTtemp <- tuneR:::readVarLength(con)
  DT <- DTtemp[1]
  EventChannel <- readBin(con, raw(0), n = 1)
  event <- substr(EventChannel, 1, 1)
  backseeked <- 0
  if (event < "8") {
    seek(con, where = -1, origin = "current")
    EventChannel <- lastEventChannel
    event <- substr(EventChannel, 1, 1)
    backseeked <- 1
  }
  eventName <- switch(
    event,
    `8` = "Note Off",
    `9` = "Note On",
    a = "Note Aftertouch",
    b = "Controller",
    c = "Program Change",
    d = "Channel Aftertouch",
    e = "Pitch Bend",
    f = "Meta or System"
  )
  if (EventChannel == "ff") {
    type <- as.character(readBin(con, raw(0), n = 1))
    elength <- tuneR:::readVarLength(con)
    eventName <-
      switch(
        type,
        `00` = "Sequence Number",
        `01` = "Text Event",
        `02` = "Copyright Notice",
        `03` = "Sequence/Track Name",
        `04` = "Instrument Name",
        `05` = "Lyric",
        `06` = "Marker",
        `07` = "Cue Point",
        `08` = "Program Name",
        `09` = "Device Name",
        `20` = "MIDI Channel Prefix",
        `21` = "MIDI Port",
        `2f` = "End of Track",
        `51` = "Set Tempo",
        `54` = "SMPTE Offset",
        `58` = "Time Signature",
        `59` = "Key Signature",
        `7f` = "Sequencer Specific",
        "Meta"
      )
    if (type > "00" && type < "10")
      eventData <- readChar(con, elength[1])
    else
      eventData <- paste(as.character(switch(
        type,
        `00` = readBin(
          con,
          integer(0),
          n = 1,
          size = elength[1],
          endian = "big"
        ),
        `20` = readBin(
          con,
          integer(0),
          n = 1,
          size = elength[1],
          endian = "big"
        ),
        `21` = readBin(
          con,
          integer(0),
          n = 1,
          size = elength[1],
          endian = "big"
        ),
        `51` = as.vector(
          readBin(
            con,
            integer(0),
            n = 3,
            size = 1,
            endian = "big",
            signed = F
          ) %*%
            c(256 ^
                2, 256, 1)
        ),
        `58` = {
          temp <- readBin(con,
                          integer(0),
                          n = 4,
                          size = 1,
                          endian = "big")
          paste0(temp[1],
                 "/",
                 2 ^ temp[2],
                 ", ",
                 temp[3],
                 " clocks/tick, ",
                 temp[4],
                 " 1/32 notes / 24 clocks")
        },
        `59` = {
          sharpflat <- readBin(con,
                               integer(0),
                               n = 1,
                               size = 1,
                               endian = "big")
          majorminor <-
            readBin(con,
                    integer(0),
                    n = 1,
                    size = 1,
                    endian = "big")
          ma <-
            paste(
              c(
                "Cb",
                "Gb",
                "Db",
                "Ab",
                "Eb",
                "Bb",
                "F",
                "C",
                "G",
                "D",
                "A",
                "E",
                "B (H)",
                "F#",
                "C#"
              ),
              "major"
            )
          mi <-
            paste(
              c(
                "Ab",
                "Eb",
                "Bb",
                "F",
                "C",
                "G",
                "D",
                "A",
                "E",
                "B (H)",
                "F#",
                "C#",
                "G#",
                "D#",
                "A#"
              ),
              "minor"
            )
          if (majorminor == 1)
            mi[sharpflat + 8]
          else
            ma[sharpflat +
                 8]
        },
        readBin(con, raw(0), n = elength[1])
      )), collapse = " x ")
    return(
      list(
        deltatime = DT,
        event = eventName,
        type = type,
        channel = NA,
        parameter1 = NA,
        parameter2 = NA,
        parameterMetaSystem = eventData,
        bytes = 2 + sum(elength) + DTtemp[2] - backseeked,
        EventChannel = EventChannel
      )
    )
  }
  if (event == "f") {
    eventName <- "System"
    elength <- tuneR:::readVarLength(con)
    seek(con, where = elength[1], origin = "current")
    return(
      list(
        deltatime = DT,
        event = eventName,
        type = NA,
        channel = NA,
        parameter1 = NA,
        parameter2 = NA,
        parameterMetaSystem = NA,
        bytes = 1 + sum(elength) + DTtemp[2] - backseeked,
        EventChannel = EventChannel
      )
    )
  }
  channel <- as.numeric(rawShift(EventChannel, 4)) / 2 ^ 4
  parameter1 <-
    readBin(
      con,
      integer(0),
      n = 1,
      size = 1,
      signed = FALSE,
      endian = "big"
    )
  parameter2 <- if (event %in% c("c", "d"))
    NA
  else
    readBin(
      con,
      integer(0),
      n = 1,
      size = 1,
      signed = FALSE,
      endian = "big"
    )
  return(
    list(
      deltatime = DT,
      event = eventName,
      type = NA,
      channel = channel,
      parameter1 = parameter1,
      parameter2 = parameter2,
      parameterMetaSystem = NA,
      # modified next line as source code caused issues with some MIDI files
      # ORIGINAL bytes = 2 + DTtemp[2] + (!(event %in% c("c", "d")) - backseeked),
      bytes = 2 + DTtemp[2] + !(event %in% c("c", "d")) - backseeked,
      EventChannel = EventChannel
    )
  )
}

StatMode = function(x){
  # function to calculate the mode of x
  
  ux = unique(x);
  # count occurences
  tab = tabulate(match(x, ux));
  return(ux[tab == max(tab)]);
}

NoteValBins = function(n=2,trip=T){
  # n represents lowest note value represented at triplet and straight level
  # note value given as 1/2^n of bar, so n = 1 gives half notes, n = 2 gives quarter notes, n = 3 gives eighth...
  
  n = round(n);

  strVal = 1/2^n;
  if(trip){
    tmpS = seq(0,1,strVal/3);
  }
  else{
    tmpS = seq(0,1,strVal);
  }
  return(tmpS);
}

NearestNoteVal = function(x,n=3,doTrip=T){
  # function to round note timings to nearest value. n is exponent used to calculate minimum note length. doTrip is whether triplets are allowed at the minimum
  # length. See NoteValBins
  
  
  noteBin = NoteValBins(n,doTrip);
  xTmp = floor(x);
  x = x - xTmp;
  
  x = sapply(x,function(i){tmp = abs(noteBin-i)==min(abs(noteBin-i))
          if(sum(tmp)>1) return(noteBin[tmp][2]) else return(noteBin[tmp][1])})
  return(xTmp + x);
}

BarToTick = function(x,tracksIn){
  mult = tracksIn$META$tick*tracksIn$META$tsig[1]*(4/tracksIn$META$tsig[2]);
  return(x*mult);
}

TickToBar = function(x,tracksIn){
  mult = tracksIn$META$tick*tracksIn$META$tsig[1]*(4/tracksIn$META$tsig[2]);
  return(x/mult);
}

BarSelector = function(tracksIn,bar1,bar2){
  tmp = lapply(tracksIn[-length(tracksIn)],function(i){tmp = NearestNoteVal(i$time/BarToTick(1,tracksIn));i[(tmp>=bar1)&(tmp<bar2),]});
  return(c(tmp,list('META'=tracksIn$META)));
}

BarNotePlot = function(tracksIn,theB1,theB2){
  layout(matrix(c(1,2),ncol = 2,nrow = 1));
  
  tmp = BarSelector(tracksIn,theB1,theB2);
  PlotTracks(tmp,xlimIn = BarToTick(c(theB1-1,theB2+1),tmp),n = (1:length(tracksIn))[(names(tracksIn)!='9')&(names(tracksIn)!='META')]);
  distOut = NoteDistribution(tmp,drums = F)$totals$length;
  barplot(distOut,names.arg = c('C','C#','D','D#','E','F','F#','G','G#','A','A#','B'),axis.lty = 1);
  
  layout(matrix(c(1),ncol = 1,nrow = 1));
  
  return(distOut);
}

NoteToNum = function(x,vectOut=T){
  tmpName = c('C','C#','D','D#','E','F','F#','G','G#','A','A#','B');
  tmpIn = toupper(x);
  
  if(vectOut){
    runSum = vector('numeric',12);
  }
  else{
    runSum = vector('numeric',length(x));
  }
  
  for(i in 1:length(tmpIn)){
    tmpCh = tmpIn[i];
    tmpNote = strsplit(tmpCh, split = "[0-9]+")[[1]];
    tmpOut = strsplit(tmpCh, split = tmpNote)[[1]][2];
    
    if(tmpNote %in% tmpName){
      tmp2 = tmpName==tmpNote;
      if(!vectOut){
        if(is.na(tmpOut)) tmpOut = 0;
        tmpOut = as.numeric(tmpOut);
        idxStart = tmpOut*12;
        runSum[i] = (0:11)[tmp2] + idxStart;
      }
      else{
        runSum = runSum + tmp2*1;
      }
    }
    else{
      stop('Note name not found.');
    }
  }
  
  return(runSum);
}

NumToNote = function(x,modVec=T){
  if(modVec){
    x = x==1;
  }
  else{
    x = x%%12 + 1;
  }
  tmpName = c('C','C#','D','D#','E','F','F#','G','G#','A','A#','B');
  return(tmpName[x]);
}

KeyDataPermute = function(x){
  tmpLen = length(x);
  outLen = 12*2*tmpLen;
  
  tmpOut = list();
  
  for(i in 0:1){
    for(j in 0:11){
      for(k in 1:tmpLen){
        thisData = x[[k]];
        curM = thisData$min;
        shiftT = j
        shiftN = (j+(i-curM)*3)%%12;
        if(shiftT == 0){
          outTruth = thisData$gtruth;
        }
        else{
          outTruth = c(tail(thisData$gtruth, shiftT), head(thisData$gtruth, -shiftT));
        }
        if(shiftN == 0){
          outNotes = thisData$notes;
        }
        else{
          outNotes = c(tail(thisData$notes, shiftN), head(thisData$notes, -shiftN));
        }
        idx = k + j*tmpLen + i*12*tmpLen;
        
        tmpOut[[idx]] = list('notes'=outNotes,'gtruth'=outTruth,'min'=i);
      }
    }
  }
  
  return(tmpOut)
}

RoundSongTimes = function(tracksIn,n=4,doLen=T,doTrip=T){
  # function to requantize MIDI tracks in time. n, doTrip are same usage as in NearestNoteVal. doLen requantizes note lengths too.
  
  data1 = tracksIn[-length(tracksIn)];
  data2 = tracksIn[length(tracksIn)];
  
  data1 = lapply(data1,function(x,m=n,o=tracksIn,p=doTrip){
                      if(nrow(x)){
                        x$time = TickToBar(x$time,o);
                        x$time = NearestNoteVal(x$time,m,p);
                        x$time = BarToTick(x$time,o);
                      }
                      return(x);});
  if(doLen){
    data1 = lapply(data1,function(x,m=n,o=tracksIn,p=doTrip){
                      if(nrow(x)){
                        tmpT = TickToBar(x$length,o);
                        tmpT = NearestNoteVal(tmpT,m,p);
                        tmpT[tmpT==0] = NoteValBins(m,p)[2];
                        x$length = BarToTick(tmpT,o);
                      }
                      return(x);
                      });
  }
  
  newMax = 0;
  for(i in 1:length(data1)){
    thisChan = data1[[i]];
    if(nrow(thisChan)){
      thisChan = max(thisChan$time + thisChan$length);
      if(thisChan>newMax) newMax = thisChan;
    }
  }
  data2$META$maxT = newMax;
  
  data2$META$resolution = c(n,doTrip);
  
  return(c(data1,data2));
}

TrackSelector = function(tracksIn,n=NULL,d=F,hasMeta=T){
  tmpLen = length(tracksIn);
  if(hasMeta) tmpLen = tmpLen - 1;
  
  if(is.null(n)){
    n = !vector('logical',tmpLen);
  }
  n = (1:tmpLen)%in%((1:tmpLen)[n]);
  isd = names(tracksIn[1:tmpLen])=='9';
  n = (n&(!isd))|(n&d);
  
  if(hasMeta) n = c(n,F);
  
  return(n);
}

PitchShift = function(tracksIn,x=0,n=NULL,d=F){
  n = TrackSelector(tracksIn,n,d);
  
  data1 = tracksIn[n];
  # data2 = tracksIn[!n];
  
  data1 = lapply(data1,function(a,b=x){
    a$note = a$note + b;
    # MIDI format does not allow negative pitches, so move to a value outside of MIDI range to fail more gracefully
    a$note[a$note<0] = 128;
    if(length(which(a$note>127)>0)) warning('new notes out of MIDI range');
    return(a);
  });
  
  tracksIn[n] = data1;
  return(tracksIn);
}

TimeShift = function(tracksIn,x=0,n=NULL,d=T){
  # function to shift track notes in time. Use in conjunction with BarToTick for easier use. Note d = FALSE will not move drum tracks, and that would be
  # big bad, so d=T is default here!
  
  n = TrackSelector(tracksIn,n,d);
  
  data1 = tracksIn[n];
  # data2 = tracksIn[!n];
  
  data1 = lapply(data1,function(a,b=x){
    a$time = a$time + b;
    if(length(which(a$time<0)>0)) warning('new notes occur at negative times!');
    # prevent negative time so can still write MIDI to output. TODO: Add option to remove notes out of range?
    a$time[a$time<0] = 0;
    return(a);
  });
  
  tracksIn[n] = data1;
  return(tracksIn);
}

SongToMatrix = function(tracksIn,n=4,doTrip=T,bulkOut=T,sustainMode=0){
  # sustainMode = 0   note length implied by consecutive notes of same pitch and velocity
  # sustainMode = 1   note length represented by negative velocity value
  # sustainMode = 2   note length represented in upper register
  tracksIn = RoundSongTimes(tracksIn,n,T,doTrip);
  dSmp = length(NoteValBins(n,doTrip))-1;
  nB = TickToBar(tracksIn$META$maxT,tracksIn);
  
  data1 = tracksIn[-length(tracksIn)];
  data2 = tracksIn[length(tracksIn)];
  
  maxVel = max(sapply(data1,function(x){return(max(x$velocity))}));
  
  data1 = lapply(data1,function(x,t=nB,d=dSmp,tra=data2){
                  tmp = TrackToMatrix(x,t,d,tra,sustainMode,maxVel);
                  });
  if(bulkOut){
    data4 = matrix(0,nrow = nrow(data1[[1]]),ncol = ncol(data1[[1]]));
    if(sustainMode!=1){
      for(i in 1:nrow(data4)){
        for(j in 1:ncol(data4)){
          data4[i,j] = data1[[which.max(unname(sapply(data1[TrackSelector(data1,hasMeta=F)],function(x,l=i,m=j){x[l,m]})))]][i,j];
        }
      }
    }
    else{
      data3 = lapply(data1,function(x){
                    x = abs(x);
                    return(x);
                    });
      
      for(i in 1:nrow(data4)){
        for(j in 1:ncol(data4)){
          data4[i,j] = data1[[which.max(unname(sapply(data3[TrackSelector(data3,hasMeta=F)],function(x,l=i,m=j){x[l,m]})))]][i,j];
        }
      }
    }
    rownames(data4) = rownames(data1[[1]]);
    colnames(data4) = colnames(data1[[1]]);
    data4 = list('0'=data4);
    data1 = c(data4,data1[!TrackSelector(data1,hasMeta = F)]);
  }
  
  data2$META$matmode = bulkOut;
  data2$META$resolution = tracksIn$META$resolution;
  data2$META$sustainmode = sustainMode;
  
  return(c(data1,data2));
}

BeatImportance = function(n=3,x=NA,i=1,opScale=0.4150375,opTrip=T,opYOff=0){
  baseVec = c(1,1/2^i);
  if(is.na(x[1])) x = c(1,1/(2^(i-opScale)));
  if(i>=n){
    if(opTrip){
      baseVec = c(1,1/2^i,1/2^i);
      opTrip = F;
    }
    else{
      return(x);
    }
  }
  
  tmpLen = length(baseVec)
  tmp = vector('numeric',length(x)*tmpLen);
  for(j in 1:length(x)){
    tmp[(tmpLen*j-(tmpLen-1)):(tmpLen*j)] = x[j]*baseVec;
  }
  return((BeatImportance(n=n,x=tmp,i=i+1,opTrip=opTrip)+2*opYOff)/(2*opYOff+1));
}

ModuloSongNotes = function(tracksIn,off=60,reArr=0,n=NULL,d=F){
  # function to convert track notes to integer pitch classes, useful for binning data.
  
  n = TrackSelector(tracksIn,n,d);
  
  data1 = tracksIn[n];
  
  data1 = lapply(data1,function(x,o=off,reA=reArr){
            tmpNotes = x$note%%12;
            tmpNotes[tmpNotes<reA] = tmpNotes[tmpNotes<reA] + 12;
            x$note = tmpNotes + o;
            return(x);
            });
  
  tracksIn[n] = data1;
  return(tracksIn);
}

NegativeHarmony = function(tracksIn,tRoot=0,n=NULL,d=F,opMed=F){
  # function to harmonically invert tracks about a harmonic centre. tRoot supplies the tonic or key of the music. setting d to TRUE will invert drums too.
  # opMed uses median instead of mean when realigning track pitches, so output pitches for each track are truer to their original pitches
  
  n = TrackSelector(tracksIn,n,d);
  data1 = tracksIn[n];
  
  tOrigin = (tRoot+3.5)%%12;
  
  data1 = lapply(data1,function(a,b=tOrigin){
    tmpNote = a$note;
    # for(i in 0:10){
    #   a$note[((tmpNote>=(i*12))&(tmpNote<(i*12+12)))] = (tOrigin+i*12) - tmpNote[((tmpNote>=(i*12))&(tmpNote<(i*12+12)))] + (tOrigin+i*12) + 12;
    # }
    
    b = b+60;
    a$note = b - a$note + b;
    if(opMed){
      beMean = median(tmpNote);
      afMean = median(a$note);
    }
    else{
      beMean = mean(tmpNote);
      afMean = mean(a$note);
    }
    a$note = a$note + round((beMean - afMean)/12)*12;
    
    a$note[a$note<0] = a$note[a$note<0] - 12*(a$note[a$note<0]%/%12);
    a$note[a$note>127] = a$note[a$note>127] - 12*((a$note[a$note>127]+4)%/%12 - 10);
    return(a);
  });
  
  tracksIn[n] = data1;
  return(tracksIn);
}

AddMeta = function(tracksIn=NULL,maxT=0,minP=0,maxP=127,tick=384,tsig=c(4,4,24,8),ksig=c(0,0),bpm=120,tmpInst=list('0'=0,'1'=0,'2'=0,'3'=0,'4'=0,'5'=0,'6'=0,'7'=0,'8'=0,'9'=0,'10'=0,'11'=0,'12'=0,'13'=0,'14'=0,'15'=0)){
  if(is.null(tracksIn)) tracksIn = list();
  
  tracksIn$META = list('maxT'=maxT,'minP'=minP,'maxP'=maxP,
                     'tick'=tick,
                     'tsig'=tsig,
                     'ksig'=ksig,
                     'bpm'=bpm,
                     'inst'=tmpInst,
                     'resolution'=NA,
                     'matmode'=NA,
                     'sustainmode'=NA);
  
  return(tracksIn);
}

AddNotes = function(chanIn=0,notesIn=NULL,timesIn=NULL,lengthsIn=NULL,velIn=NULL,tracksIn=NULL){
  if(is.null(tracksIn)) tracksIn = AddMeta();
  
  if(typeof(notesIn)=='character') notesIn = NoteToNum(notesIn,F);
  timesIn = round(BarToTick(timesIn,tracksIn));
  if(is.null(velIn)) lengthsIn = rep(0.25,length(notesIn));
  lengthsIn = round(BarToTick(lengthsIn,tracksIn));
  if(is.null(velIn)) velIn = rep(127,length(notesIn));
  
  newData = data.frame('time'=timesIn,'note'=notesIn,'velocity'=velIn,'length'=lengthsIn);
  if(chanIn %in% names(tracksIn)){
    theTrack = which(names(tracksIn)==chanIn);
    tracksIn[[theTrack]] = rbind(tracksIn[[theTrack]],newData);
    tracksIn[[theTrack]] = tracksIn[[theTrack]][order(tracksIn[[theTrack]]$time),];
  }
  else{
    tracksIn$TMP = newData;
    tmpNames = names(tracksIn);
    tmpNames[tmpNames=='TMP'] = as.character(chanIn);
    names(tracksIn) = tmpNames;
  }
  
  
  tmpSel = names(tracksIn)=='META'
  dataMeta = tracksIn[tmpSel];
  data1 = tracksIn[!tmpSel];
  data1 = data1[order(names(data1))];
  
  return(c(data1,dataMeta));
}

LengthDistribution = function(tracksIn,normMode=0){
  # normMode == 0      raw counts
  # normMode == 1      normalise to max within track
  # normMode == 2      normalise to max across all tracks
  lBinning = NoteValBins(4,F)[2];
  tmpLen = length(tracksIn)-1;
  
  outPutL = matrix(0,ncol = tmpLen,nrow = 18,dimnames = list(c(seq(0,1,1/16),'1<'),names(tracksIn[-length(tracksIn)])));
  outPutT = matrix(0,ncol = tmpLen,nrow = 16,dimnames = list(seq(0,15/16,1/16),names(tracksIn[-length(tracksIn)])));
  
  for(i in 1:tmpLen){
    outPutL[,i] = hist(TickToBar(tmp[[i]]$length,tmp),breaks = c(seq(-lBinning,1,lBinning)+lBinning/2,500),plot = F)$counts;
    tDist = hist(TickToBar(tmp[[i]]$time,tmp)%%1,breaks = c(seq(0,1+lBinning,lBinning)-lBinning/2),plot = F)$counts;
    tDist[1] = tDist[1] + tDist[17];
    outPutT[,i] = tDist[1:16];
  }
  
  theSums = colSums(outPutT);
  outPutL = outPutL/theSums[col(outPutL)];
  outPutT = outPutT/theSums[col(outPutT)];
  return(list('lengths'=outPutL,'times'=outPutT,'N'=theSums));
}

MeanPolyphony = function(tracksIn,norm01 = T){
  trkMat = SongToMatrix(tracksIn,bulkOut = F);
  tmpLen = length(tracksIn)-1;
  
  output = vector('numeric',tmpLen);
  
  for(i in 1:tmpLen){
    tmpTrk = rowSums(trkMat[[i]]>0);
    output[i] = mean(tmpTrk[tmpTrk>0]);
  }
  names(output) = names(tracksIn[1:tmpLen]);
  
  if(norm01){
    output = output-median(output);
    output[output>0] = output[output>0]/max(output);
    output[output<0] = -output[output<0]/min(output);
    output = (output/2)+0.5;
  }
  
  return(output);
}

NotesPerBar = function(tracksIn,norm01 = T){
  trkMat = SongToMatrix(tracksIn,bulkOut = F);
  tmpLen = length(tracksIn)-1;
  tickPerBar = 2^trkMat$META$resolution[1];
  tmpRes = trkMat$META$resolution[2]*2*tickPerBar + tickPerBar;
  
  output = vector('numeric',tmpLen);
  
  for(i in 1:tmpLen){
    nNotes = length(tracksIn[[i]]$note);
    tmpTrk = which(rowSums(trkMat[[i]]>0)>0);
    nBarPlay = (max(tmpTrk) - min(tmpTrk))/tmpRes;
    output[i] = nNotes/nBarPlay;
  }
  
  if(norm01){
    output = output-median(output);
    output[output>0] = output[output>0]/max(output);
    output[output<0] = -output[output<0]/min(output);
    output = (output/2)+0.5;
  }
  
  return(output);
}

MatrixToSong = function(matIn){
  output = AddMeta(tick = matIn$META$tick,tsig = matIn$META$tsig,ksig = matIn$META$ksig,bpm = matIn$META$bpm);
  timeMult = 2^matIn$META$resolution[1]*(1+2*matIn$META$resolution[2]);
  if(matIn$META$sustainmode==0){
    tmpMelody = matIn$`0`;
    nonZeroIdx = ElmtToIdx(which(tmpMelody>0),nrow(tmpMelody));
    uniqueCol = unique(nonZeroIdx[,2]);
    k = 0;
    for(j in uniqueCol){
      if(j==68 && k ==0){
        Sys.sleep(0.1);
        k = 1;
      }
      tmpSubMat = nonZeroIdx[nonZeroIdx[,2]==j,1];
      tmpSubMat = tmpSubMat[order(tmpSubMat,decreasing = T)];
      for(i in tmpSubMat){
        if(i!=tmpSubMat[1]){
          thisEl = tmpMelody[i,j];
          if(thisEl!=tmpMelody[i+1,j]){
            # ADD NOTE HERE WITH LENGTH NOTELEN AND VELOCITY THISEL
            output = AddNotes(0,j-1,(noteTime-1)/timeMult,noteLen/timeMult,thisEl,output);
            noteLen = 1;
            noteTime = i;
          }
          else{
            noteLen = noteLen + 1;
            noteTime = i;
          }
        }
        else{
          noteLen = 1;
          noteTime = i;
          thisEl = tmpMelody[i,j];
        }
      }
      output = AddNotes(0,j-1,(noteTime-1)/timeMult,noteLen/timeMult,thisEl,output);
      noteLen = 1;
      noteTime = i;
    }
  }
  
  return(output);
}

ElmtToIdx = function(n,matRow){
  output = matrix(0,length(n),2);
  i = (n-1)%%matRow + 1;
  j = (n-1)%/%matRow + 1;
  output[,1] = i;
  output[,2] = j;
  return(output);
}

UpdateMinMaxMeta = function(){
  # TODO
}

InstrumentList = function(n,outCol = 1:4){
  theList = data.frame('Patch ID'=0:127,
                       'Patch'=c('Acoustic Grand Piano'
                                    ,'Bright Acoustic Piano'
                                    ,'Electric Grand Piano'
                                    ,'Honky-tonk Piano'
                                    ,'Electric Piano 1'
                                    ,'Electric Piano 2'
                                    ,'Harpsichord'
                                    ,'Clavi'
                                    ,'Celesta'
                                    ,'Glockenspiel'
                                    ,'Music Box'
                                    ,'Vibraphone'
                                    ,'Marimba'
                                    ,'Xylophone'
                                    ,'Tubular Bells'
                                    ,'Dulcimer'
                                    ,'Drawbar Organ'
                                    ,'Percussive Organ'
                                    ,'Rock Organ'
                                    ,'Church Organ'
                                    ,'Reed Organ'
                                    ,'Accordion'
                                    ,'Harmonica'
                                    ,'Tango Accordion'
                                    ,'Acoustic Guitar (nylon)'
                                    ,'Acoustic Guitar (steel)'
                                    ,'Electric Guitar (jazz)'
                                    ,'Electric Guitar (clean)'
                                    ,'Electric Guitar (muted)'
                                    ,'Overdriven Guitar'
                                    ,'Distortion Guitar'
                                    ,'Guitar harmonics'
                                    ,'Acoustic Bass'
                                    ,'Electric Bass (finger)'
                                    ,'Electric Bass (pick)'
                                    ,'Fretless Bass'
                                    ,'Slap Bass 1'
                                    ,'Slap Bass 2'
                                    ,'Synth Bass 1'
                                    ,'Synth Bass 2'
                                    ,'Violin'
                                    ,'Viola'
                                    ,'Cello'
                                    ,'Contrabass'
                                    ,'Tremolo Strings'
                                    ,'Pizzicato Strings'
                                    ,'Orchestral Harp'
                                    ,'Timpani'
                                    ,'String Ensemble 1'
                                    ,'String Ensemble 2'
                                    ,'SynthStrings 1'
                                    ,'SynthStrings 2'
                                    ,'Choir Aahs'
                                    ,'Voice Oohs'
                                    ,'Synth Voice'
                                    ,'Orchestra Hit'
                                    ,'Trumpet'
                                    ,'Trombone'
                                    ,'Tuba'
                                    ,'Muted Trumpet'
                                    ,'French Horn'
                                    ,'Brass Section'
                                    ,'SynthBrass 1'
                                    ,'SynthBrass 2'
                                    ,'Soprano Sax'
                                    ,'Alto Sax'
                                    ,'Tenor Sax'
                                    ,'Baritone Sax'
                                    ,'Oboe'
                                    ,'English Horn'
                                    ,'Bassoon'
                                    ,'Clarinet'
                                    ,'Piccolo'
                                    ,'Flute'
                                    ,'Recorder'
                                    ,'Pan Flute'
                                    ,'Blown Bottle'
                                    ,'Shakuhachi'
                                    ,'Whistle'
                                    ,'Ocarina'
                                    ,'Lead 1 (square)'
                                    ,'Lead 2 (sawtooth)'
                                    ,'Lead 3 (calliope)'
                                    ,'Lead 4 (chiff)'
                                    ,'Lead 5 (charang)'
                                    ,'Lead 6 (voice)'
                                    ,'Lead 7 (fifths)'
                                    ,'Lead 8 (bass + lead)'
                                    ,'Pad 1 (new age)'
                                    ,'Pad 2 (warm)'
                                    ,'Pad 3 (polysynth)'
                                    ,'Pad 4 (choir)'
                                    ,'Pad 5 (bowed)'
                                    ,'Pad 6 (metallic)'
                                    ,'Pad 7 (halo)'
                                    ,'Pad 8 (sweep)'
                                    ,'FX 1 (rain)'
                                    ,'FX 2 (soundtrack)'
                                    ,'FX 3 (crystal)'
                                    ,'FX 4 (atmosphere)'
                                    ,'FX 5 (brightness)'
                                    ,'FX 6 (goblins)'
                                    ,'FX 7 (echoes)'
                                    ,'FX 8 (sci-fi)'
                                    ,'Sitar'
                                    ,'Banjo'
                                    ,'Shamisen'
                                    ,'Koto'
                                    ,'Kalimba'
                                    ,'Bag pipe'
                                    ,'Fiddle'
                                    ,'Shanai'
                                    ,'Tinkle Bell'
                                    ,'Agogo'
                                    ,'Steel Drums'
                                    ,'Woodblock'
                                    ,'Taiko Drum'
                                    ,'Melodic Tom'
                                    ,'Synth Drum'
                                    ,'Reverse Cymbal'
                                    ,'Guitar Fret Noise'
                                    ,'Breath Noise'
                                    ,'Seashore'
                                    ,'Bird Tweet'
                                    ,'Telephone Ring'
                                    ,'Helicopter'
                                    ,'Applause'
                                    ,'Gunshot'),
                       'Group ID'=0:127 %/% 8,
                       'Group'= rep(c('Piano'
                                    ,'Chromatic Percussion'
                                    ,'Organ'
                                    ,'Guitar'
                                    ,'Bass'
                                    ,'Strings'
                                    ,'Ensemble'
                                    ,'Brass'
                                    ,'Reed'
                                    ,'Pipe'
                                    ,'Synth Lead'
                                    ,'Synth Pad'
                                    ,'Synth Effects'
                                    ,'Ethnic'
                                    ,'Percussive'
                                    ,'Sound Effects'),each=8),
                       stringsAsFactors = F
  );
  
  return(theList[n+1,outCol]);
}

GroupTrackInstruments = function(tracksIn,defaultInst = c(0,8,16,24,32,40,48,56,64,0,72,80,88,96,104,112),nameOut = 2){
  # function to merge tracks that use instruments from the same instrument class, according to InstrumentList. Note that this function ignores (and removes)
  # tracks using instruments from the "Sound Effects" instrument group as in doing so, the number of tracks can be limited to 16 and so remains General MIDI
  # compliant. If nameOut is 0, instrument class numbers are used as track names; if 1, then instrument names are used as track names; if 2, then instrument
  # class names are used as track names
  
  if(!(nameOut %in% c(0,1,2))){
    stop('nameOut is wrong');
  }
  tmpMeta = tracksIn$META;
  trkName = names(tracksIn);
  insClass = sapply(tracksIn$META$inst,function(x){StatMode(x)%/%8});
  
  tracksOut = list();
  
  k = 0;
  for(i in 1:16){
    if(i!=10){
      toMerge = which(insClass==(i-1-k));
      toMerge = toMerge[(toMerge!=10)&((toMerge-1)%in%trkName)];
      if(length(toMerge>0)){
        for(j in 1:length(toMerge)){
          if(j==1){
            tmpTrk = tracksIn[[toMerge[j]]];
          }
          else{
            tmpTrk = rbind(tmpTrk,tracksIn[[toMerge[j]]]);
          }
        }
        tmpTrk = tmpTrk[order(tmpTrk[,1]),];
      }
      else{
        tmpTrk = data.frame('time'=c(),'note'=c(),'velocity'=c(),'length'=c());
      }
      thisInst = defaultInst[i];
      if(nameOut){
        thisName = toupper(InstrumentList(thisInst,2*nameOut));
      }
      else{
        thisName = i-1;
      }
    }
    else{
      k = 1;
      thisInst = defaultInst[i];
      tmpTrk = tracksIn[trkName=='9'][[1]];
      if(nameOut){
        thisName = 'DRUMS';
      }
      else{
        thisName = i-1;
      }
    }
    tmpMeta$inst[[i]] = thisInst;
    tracksOut$TMP = tmpTrk;
    names(tracksOut)[(names(tracksOut)=='TMP')] = thisName;
  }
  
  
  
  return(c(tracksOut,list('META'=tmpMeta)));
}

EqualEl = function(x,tol=10e-10){
  # checks if all elements in a vector are equal. comparisons are not transitive for FP ops, so need to check max and min rather than eg. each elmt to first
  return(abs(max(x)-min(x))<tol);
}

NBitCount = function(counter=0,inc=1,base=2,warnOn=T){
  # N-bit counter with userdefined increment and base
  
  n = length(counter);
  
  carry = inc;
  for(i in n:1){
    oldCar = carry;
    carry = (counter[i] + carry) %/% (base);
    counter[i] = (counter[i] + oldCar) %% (base);
  }
  if((carry>0)&(warnOn)) warning('bit overflow error')
  return(counter);
}

ChordDataGen = function(chord,rootLabel,otherLab=data.frame(NULL)){
  # takes a vector of notes and permutes it to generate a dataset for training a yet-to-be-built chord identifier, maintaining relative order of notes.
  # TODO horribly inefficient, also consider adding chord inversions if enough savings can be made. Maybe bound the chord to an octave range provided by user,
  # eg. chord cannot span > 2 oct?
  
  maxMIDI = 127;
  n = length(chord);
  chord = chord[order(chord)];
  
  
  tmpLab = (rootLabel - chord[1])%%12;
  tmpCh = chord %% 12;
  tmpCh = tmpCh - tmpCh[1];

  # loop up to first valid permutation
  for(i in 2:n){
    while(tmpCh[i]<tmpCh[i-1]){
      tmpCh[i] = tmpCh[i] + 12;
    }
  }
  
  chord = tmpCh;
  rootLabel = tmpLab;
  
  # determine maximum number of octaves that can be added
  maxOct = 0;
  while((tmpCh[n]+12)<(maxMIDI+1)){
    tmpCh = tmpCh + 12;
    maxOct = maxOct + 1;
  }
  
  out = data.frame(NULL);
  octave = vector('numeric',n);
  
  pBar = txtProgressBar(min = 0,max = (maxOct+1)^length(chord),initial = 0,width = 75,style = 3);
  
  while(T){
    # loop through all notes
    i = 0;
    tmpCh = chord;
    while(tmpCh[n]<maxMIDI){
      tmpCh = chord + octave*12 + i;
      tmpLab = rootLabel + i;
      out = rbind(out,cbind(t(tmpCh),data.frame('root'=tmpLab)));
      i = i + 1;
    }
    
    octave = NBitCount(octave,1,maxOct);
    # WE ARE STILL REPEATING OPERATIONS DESPITE THE FIRST IF STATEMENT BELOW, THEREFORE:
    # MERGE THIS INTO THIRD IF;
    # ADD A GROWING LIST OF CASES ALREADY COMPLETED AND CHECK;
    # PERFORM THE TWO IF STATEMENTS IN A WHILE LOOP ON WHETHER THE PROPOSED CASE IS VALID;
    # ADD AN END FLAG TO CHECK FOR END CONS IF NECESSARY
    if(EqualEl(octave)){
      # cases where octaves are added equally to each note of the chord are already handled in the sweep above, so skip
      octave = NBitCount(octave,1,maxOct);
    }
    if((octave[1]==0)&&(EqualEl(octave))){
      # if after updating octave overflows, all cases have been accounted for
      break;
    }
    # TODO
    print('stop this nonsense and fix the todo stuff')
    if(F){
      # check if octave case already accounted for TODO
      octave = NBitCount(octave,1,maxOct);
    }
    thisIt = sum(octave*(maxOct+1)^((length(chord)-1):0));
    setTxtProgressBar(pBar,thisIt);
  }
  
  if(nrow(otherLab)){
    cbind(out,otherLab);
  }

  return(out);
}