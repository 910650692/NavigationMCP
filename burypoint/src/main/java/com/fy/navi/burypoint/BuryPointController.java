package com.fy.navi.burypoint;

import com.android.utils.log.Logger;

public class BuryPointController {

    private static final String TAG = "BuryPointController";
    private static volatile BuryPointController instance;

    public static BuryPointController getInstance() {
        if (null == instance){
            synchronized (BuryPointController.class){
                if (null == instance){
                    instance = new BuryPointController();
                }
            }
        }
        return instance;
    }

    public void setBuryInfo(){
        Logger.d(TAG, "setBuryInfo");
    }

    public void getBuryInfo(){
        Logger.d(TAG, "getBuryInfo");
    }
}
