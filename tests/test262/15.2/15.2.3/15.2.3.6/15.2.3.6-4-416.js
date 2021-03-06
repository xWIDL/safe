  function testcase() 
  {
    var appointment = {
      
    };
    Object.defineProperty(appointment, "startTime", {
      value : 1001,
      writable : false,
      enumerable : false,
      configurable : true
    });
    Object.defineProperty(appointment, "name", {
      value : "NAME",
      writable : false,
      enumerable : false,
      configurable : true
    });
    var meeting = Object.create(appointment);
    Object.defineProperty(meeting, "conferenceCall", {
      value : "In-person meeting",
      writable : false,
      enumerable : false,
      configurable : true
    });
    var teamMeeting = Object.create(meeting);
    var verifyTimeProp = false;
    var verifyNameProp = false;
    var verifyCallProp = false;
    for(var p in teamMeeting)
    {
      if (p === "startTime")
      {
        verifyTimeProp = true;
      }
      if (p === "name")
      {
        verifyNameProp = true;
      }
      if (p === "conferenceCall")
      {
        verifyCallProp = true;
      }
    }
    var hasOwnProperty = ! teamMeeting.hasOwnProperty("name") && ! teamMeeting.hasOwnProperty("startTime") && ! teamMeeting.hasOwnProperty("conferenceCall");
    return hasOwnProperty && ! verifyTimeProp && ! verifyNameProp && ! verifyCallProp;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  