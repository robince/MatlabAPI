% MatlabAPI compile routine for Intel Fortran 9.1 with MSVS 2005 linker
%*************************************************************************************
% 
%  MATLAB (R) is a trademark of The Mathworks (R) Corporation
% 
%  Filename:    MatlabAPI_build_intelf91msvs2005.m
%  Programmer:  James Tursa
%  Version:     1.01
%  Date:        December 11, 2009
%  Copyright:   (c) 2009 by James Tursa, All Rights Reserved
%
%  This code uses the BSD License:
%
%  Redistribution and use in source and binary forms, with or without 
%  modification, are permitted provided that the following conditions are 
%  met:
%
%     * Redistributions of source code must retain the above copyright 
%       notice, this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above copyright 
%       notice, this list of conditions and the following disclaimer in 
%       the documentation and/or other materials provided with the distribution
%      
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
%  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
%  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
%  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
%  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
%  POSSIBILITY OF SUCH DAMAGE.
%
%  Change Log:
%  2009/Oct/27 --> Initial Release
%  2009/Dec/11 --> Added MatlabAPI_implicit.f to build
% 
%*************************************************************************************
%%
disp('MatlabAPI build script for Gfortan maci64')
disp(' ')
disp('... Compiling module MatlabAPImx')
mex -c -DNOCOMPLEX16 -DNOREAL16 MatlabAPImx.f
disp('... Compiling module MatlabAPImex')
mex -c  MatlabAPImex.f
disp('... Compiling module MatlabAPIeng')
mex -c MatlabAPIeng.f
disp('... Compiling module MatlabAPImat')
mex -c MatlabAPImat.f
%%
disp('... Compiling MatlabAPI_real.f example')
mex  MatlabAPI_real.f MatlabAPImex.o MatlabAPImx.o
disp('... Compiling MatlabAPI_complex.f example')
mex MatlabAPI_complex.f MatlabAPImex.o MatlabAPImx.o
disp('... Compiling MatlabAPI_getset.f example')
mex MatlabAPI_getset.f MatlabAPImex.o MatlabAPImx.o
disp('... Compiling MatlabAPI_implicit.f example')
mex MatlabAPI_implicit.f MatlabAPImex.o MatlabAPImx.o
disp('... Compiling MatlabAPI_matfile.f example')
mex MatlabAPI_matfile.f MatlabAPImex.o MatlabAPImx.o MatlabAPImat.o
disp('... Compiling MatlabAPI_engine.f example')
%%
options = [matlabroot '/bin/engopts.sh'];
mex('-v','-f', options, 'MatlabAPI_engine.f', 'MatlabAPIeng.o', 'MatlabAPImx.o')
disp('... Done!')
