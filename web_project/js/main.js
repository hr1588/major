  function checkAll(){
        if(!checkUserId(form.userId.value)) {
            return false;
         }
         if(!checkPassword(form.password1.value, form.password2.value)){
            return false;
         }
         if(!checkName(form.name.value)){
            return false;
         }
         if(!checkEmail(form.email.value)){
            return false;
         }
        return true; //모든 요소가 true일때 회원가입 가능
    }
    function checkPassword(password1, password2){
        //비밀번호가 입력되었는지ㅣ 확인
        if(!checkExistData(form.password1.value, "비밀번호를 ")){
            return false;
        }
        //비밀번호 확인에 
        if(!checkExistData(form.password2.value, "비밀번호 확인을 ")){
            return false;
        }
        const passwordRegExp= /^[a-zA-Z0-9]{4,12}$/ ; //비밀번호 유효성 검사, 집합기호는 입력범위(4~12자)
        if(!passwordRegExp.test(password1)){
            alert("비밀번호는 영문대소문자와 숫자 4~12자리로 입력해주세요");
            form.password1.value="";
            form.password1.focus();
            return false;
        }
        if(password1 != password2){
            alert("비밀번호가 일치하지 않습니다");
            form.password1.value =""
            form.password2.value =""
            form.password1.focus();
            return false;
        }
        return true;//확인이 완련되면 
    }
    function checkUserId(id){
        if(!checkExistData(form.userId.value, "아이디를 ")){
            return false;
        }
        //JS의 정규표현식 
        // /는 시작과 끝을 표시
        // [ ] : 문자셑 ex) [a-z]는 소문자 a~z를 입력
        // {min, max } : 문자열의 길이
        // ^ : 문장의 시작
        // $ : 문장의 끝
        const idRegExp = /^[a-zA-Z0-9]{4,12}$/ ;
        if(!idRegExp.test(id)){
            alert("아이디는 영문대소문자와 숫자 4~12자리로 입력해주세요");
            form.userId.value="";
            form.userId.focus();
            return false;
        }
        return true;
    }
    function checkEmail(email){
        if(!checkExistData(form.email.value, "이메일을 ")){
            return false;
        }
        return true;
    }
    function checkName(name){
        if(!checkExistData(form.name.value, "이름을 ")){
            return false;
        }
        return true;
    }
    function checkExistData(value, dataName){
        if(value==""){
            alert(dataName + "입력해주세요");
            return false;
        }
        return true;
    }

    
    

    