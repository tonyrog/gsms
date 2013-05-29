
Send SMS with gsms

    gsms_router:send([{addr,"<number>"}], "Hello").

Receive SMS

    {ok,Ref} = gsms_router:subscribe([]).
    Pdu = receive {gsms,Ref,Pdu1} -> Pdu1 end.
    Text = Pdu#gsms_deliver_pdu.ud.


