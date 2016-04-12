PRO STR_IDL_TO_TEX, str, stro

stro  = str
idl_token = ['!U','!D','!N','!9m','!X']
ntoken    = N_ELEMENTS(idl_token)
tex_token = ['$^{','$_{','}$','$\mu','$']
stro = ''
maxpass = 10
FOR ipass = 0, maxpass-1L DO BEGIN
  FOR itk = 0L, ntoken-1L DO BEGIN
     itoken = idl_token[itk]
     ttoken = tex_token[itk]
     ltoken = STRLEN(itoken)
     ipos   = STRPOS(str,itoken)
     IF (ipos lt 0) THEN continue 
     strn = STRMID(str,0,ipos) + ttoken + STRMID(str,ipos+ltoken,STRLEN(str))
     str = strn
  ENDFOR
ENDFOR
stro = str
END
PRO STR_TEX_TO_IDL, str, stro

stro  = str
idl_token = ['!U','!D','!N','!9m','!X']
ntoken    = N_ELEMENTS(idl_token)
tex_token = ['$^{','$_{','}$','$\mu','$']
stro = ''
maxpass = 10
FOR ipass = 0, maxpass-1L DO BEGIN
  FOR itk = 0L, ntoken-1L DO BEGIN
     itoken = idl_token[itk]
     ttoken = tex_token[itk]
     ltoken = STRLEN(ttoken)
     ipos   = STRPOS(str,ttoken)
     IF (ipos lt 0) THEN continue 
     strn = STRMID(str,0,ipos) + itoken + STRMID(str,ipos+ltoken,STRLEN(str))
     str = strn
  ENDFOR
ENDFOR
stro = str

END
