package com.sgm.navi.hmi.search.mainsearch;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.navi.ContinueNaviDialog;
import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.logicpaket.activate.ActivatePackage;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.ui.base.BaseModel;

public class MainSearchModel extends BaseModel<MainSearchViewModel> {
    private final CalibrationPackage mCalibrationPackage;
    private final ActivatePackage mActivatePackage;

    public MainSearchModel() {
        mCalibrationPackage = CalibrationPackage.getInstance();
        mActivatePackage = ActivatePackage.getInstance();
    }


    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mCalibrationPackage.powerType();
    }

    /**
     * 获取AppKey并存储
     */
    public void getAppKey(){
        if(!ConvertUtils.isNull(mActivatePackage)){
            if(ConvertUtils.isEmpty(mActivatePackage.getAppKeyFromDB())){
                mActivatePackage.getAppKeyFromNet(new NetQueryManager.INetResultCallBack<AppKeyResponse>() {
                    @Override
                    public void onSuccess(AppKeyResponse response) {
                        if(ConvertUtils.isNull(response)) return;
                        if(BuildConfig.DEBUG){
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"AppKey: ");
                        }
                        NetQueryManager.getInstance().saveAppSecurity(response.getMAppKey());
                    }

                    @Override
                    public void onFailed() {
                        if(BuildConfig.DEBUG){
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getAppKeyFromNet onFailed");
                        }
                    }
                });
            }else{
                if(BuildConfig.DEBUG){
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getAppKeyFromDB: "+mActivatePackage.getAppKeyFromDB());
                }
            }
        }else{
            if(BuildConfig.DEBUG){
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mActivatePackage is null");
            }
        }
    }
}
