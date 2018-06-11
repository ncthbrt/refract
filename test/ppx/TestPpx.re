let () = {
  let a =[%refract "/hello1/{:string}"](name => { 
    print_endline("hello unlabelled " ++ name); 
    Refract.Prism.handled; 
  });
  /* ignore( 
    Refract.switch_( 
      [ 
      
        [%refract "/hello2/{name:string}/{surname:string}"]((~name, ~surname) => { 
          print_endline(name ++ surname); 
          Refract.Prism.handled; 
        }), 
      ], 
      Obj.magic(), 
    ), 
  );  */

  Io.run(); 
};
  /* [%refract.post "/hello3/{name:string}"]((~name) => { 
       let%mesh body = Refract.Request.bodyString; 
       print_endline("hello labelled " ++ name ++ " " ++ body); 
       Refract.Prism.handled; 
     }), */ 
  /* [%route.post "/hello4/name:string?offset:int=?&limit:uint=?&cached=?"]( 
       (~name, ~offset=0, ~limit=10, ~cached) => { 
       let%mesh body = Refract.Request.bodyString; 
       print_endline( 
         "hello labelled " 
         ++ name 
         ++ " " 
         ++ body 
         ++ " " 
         ++ string_of_int(limit) 
         ++ "cached: " 
         ++ string_of_bool(cached), 
       ); 
       Refract.Prism.handled; 
     }), */ 
  /* { 
       request: { 
         resource: "/hello4/nick?limit=4", 
         headers: [], 
         method: Post, 
         body: Obj.magic(), 
       }, 
       response: { 
         status: NotFound, 
         headers: [], 
       }, 
     }, */ 