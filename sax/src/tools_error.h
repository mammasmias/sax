
#define ERROR(MSG) call tools_er(MSG,__FILE__,__LINE__,0)
#define WARNING(MSG) call tools_er(MSG,__FILE__,__LINE__,1)
#define DEBUG(MSG) call tools_er(MSG,__FILE__,__LINE__,2)

#define IOCHECK(unit,iostat) if(iostat/=0) call tools_ioerr(iostat,unit,__FILE__,__LINE__)

