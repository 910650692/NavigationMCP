package com.fy.navi.burypoint.controller;

import com.android.utils.log.Logger;
import com.fy.navi.burypoint.bean.BuryProperty;

public class BuryPointController {

    private static final String TAG = "BuryPointController";
    private static volatile BuryPointController instance;
    private BuryProperty mBuryProps = new BuryProperty();
    private String eventName = "";

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

    public void setEventName(final String eventName){
        Logger.d(TAG, "setEventName");
        this.eventName = eventName;
    }

    public String getEventName(){
        Logger.d(TAG, "getEventName");
        String tempEventName = eventName;
        eventName = "";
        return tempEventName;
    }

    public void setBuryProps(final BuryProperty buryProp){
        Logger.d(TAG, "setBuryProps");
        if(buryProp != null){
            mBuryProps = buryProp;
        }
    }

    public BuryProperty getBuryProps(){
        Logger.d(TAG, "getBuryProps");
        if(null != mBuryProps && null != mBuryProps.getParams() && !mBuryProps.getParams().isEmpty()){
            Logger.d(TAG, "getBuryProps params size:" + mBuryProps.getParams().size());
            BuryProperty buryProperty = mBuryProps;
            mBuryProps = null;
            return buryProperty;
        }
        return null;
    }
}
