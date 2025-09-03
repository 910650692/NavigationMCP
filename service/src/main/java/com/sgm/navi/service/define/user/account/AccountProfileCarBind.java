package com.sgm.navi.service.define.user.account;

import com.autonavi.gbl.user.account.model.AccountProvider;
import com.autonavi.gbl.user.account.model.UserCar;

import java.util.ArrayList;

public class AccountProfileCarBind {
    public int level = 0;
    public int gender = 0;
    public int checkin_count = 0;
    public String birthday = "";
    public String description = "";
    public ArrayList<AccountProviderCarBind> providers = new ArrayList();
    public boolean carLoginFlag = false;
    public UserCarBindInfo car = new UserCarBindInfo();

    public AccountProfileCarBind(int level, int gender, int checkin_count, String birthday,
                                 String description, ArrayList<AccountProviderCarBind> providers, boolean carLoginFlag, UserCarBindInfo car) {
        this.level = level;
        this.gender = gender;
        this.checkin_count = checkin_count;
        this.birthday = birthday;
        this.description = description;
        this.providers = providers;
        this.carLoginFlag = carLoginFlag;
        this.car = car;
    }
}
