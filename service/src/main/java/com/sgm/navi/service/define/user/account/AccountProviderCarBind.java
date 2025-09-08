package com.sgm.navi.service.define.user.account;

public class AccountProviderCarBind {
    public int provider;
    public int auth_id;
    public String auth_username;

    public AccountProviderCarBind() {
        this.provider = 0;
        this.auth_id = 0;
        this.auth_username = "";
    }

    public AccountProviderCarBind(int providerLiteObj, int auth_idLiteObj, String auth_usernameLiteObj) {
        this.provider = providerLiteObj;
        this.auth_id = auth_idLiteObj;
        this.auth_username = auth_usernameLiteObj;
    }
}
