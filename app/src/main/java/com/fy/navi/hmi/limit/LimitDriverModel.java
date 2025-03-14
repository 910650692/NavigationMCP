package com.fy.navi.hmi.limit;


import static com.fy.navi.service.define.map.MapTypeId.MAIN_SCREEN_MAIN_MAP;

import android.os.Handler;
import android.os.Looper;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.aos.RestrictedParam;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityItemBean;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.fy.navi.service.logicpaket.aos.IAosRestrictedObserver;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: QiuYaWei
 * Date: 2025/2/7
 * Description: [TODO]
 */
public class LimitDriverModel extends BaseModel<LimitDriverViewModel> implements IAosRestrictedObserver {
    private static final String TAG = "LimitDriverModel";
    private static Handler mHandler;
    private Long mLimitQueryTaskId;
    private String mCurrentCityCode;
    private static final int FAIL_TIME = 10 * 1000;
    private Runnable loadingFail = new Runnable() {
        @Override
        public void run() {
            mLimitQueryTaskId = null;
            mViewModel.loadingFail();
        }
    };

    @Override
    public void onCreate() {
        super.onCreate();
        mHandler = new Handler(Looper.getMainLooper());
        AosRestrictedPackage.getInstance().addRestrictedObserver(IAosRestrictedObserver.KEY_OBSERVER_LIMIT_VIEW, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        AosRestrictedPackage.getInstance().removeRestrictedObserver(IAosRestrictedObserver.KEY_OBSERVER_LIMIT_VIEW);
    }

    public void queryLimitPolicyByCityCode(String cityCode) {
        Logger.d(TAG, "queryRetry cityCode:" + cityCode);
        String license = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        if (license == null || license.isEmpty()) {
            Logger.d(TAG, "No license plate set");
            return;
        }
        mCurrentCityCode = cityCode;
        RestrictedParam restrictedParam = new RestrictedParam();
        // 请求根据车型等信息获取的规则 type = 7 请求城市全部规则 type = 8 请求城市列表 type = 9 根据规则请求数据
        restrictedParam.setRestrict_type(7);
        restrictedParam.setPlate(license);
        restrictedParam.setAdcodes(cityCode);
        mLimitQueryTaskId = AosRestrictedPackage.getInstance().queryRestrictedInfo(restrictedParam);
        mHandler.postDelayed(loadingFail, FAIL_TIME);
    }

    public void queryRetry() {
        if (mCurrentCityCode != null && !mCurrentCityCode.isEmpty()) {
            Logger.d(TAG, "queryRetry cityCode:" + mCurrentCityCode);
            queryLimitPolicyByCityCode(mCurrentCityCode);
        }
    }

    public double TransCityLatAndLon(double input) {
        double scaleFactor = 1000000.0;
        return input / scaleFactor;
    }

    @Override
    public void queryLimitResult(RouteRestrictionParam param) {
        // 限行信息查询成功后更新UI
        if (mLimitQueryTaskId == null) {
            return;
        }
        RestrictedArea restrictedAreaDetail = param.getRestrictedArea();
        if (mLimitQueryTaskId == restrictedAreaDetail.getRequestId()) {
            mHandler.removeCallbacks(loadingFail);
            if (restrictedAreaDetail.cityNames == null || restrictedAreaDetail.cityNames.isEmpty()) {
                if (mCurrentCityCode != null && !mCurrentCityCode.isEmpty()) {
                    ArrayList<String> cityName = new ArrayList<>();
                    cityName.add(MapDataPackage.getInstance()
                            .getCityInfo(Integer.parseInt(mCurrentCityCode)).name);
                    restrictedAreaDetail.setCityNames(cityName);
                }
            }
            if (mCurrentCityCode != null && !mCurrentCityCode.isEmpty()) {
                CityDataInfo cityItemBean= MapDataPackage.getInstance().getCityInfo(Integer.parseInt(mCurrentCityCode));
                MapPackage.getInstance().setMapCenter(MAIN_SCREEN_MAIN_MAP,
                        new GeoPoint(TransCityLatAndLon(cityItemBean.cityX), TransCityLatAndLon(cityItemBean.cityY)));
            }
            RoutePackage.getInstance().drawRestrictionForLimit(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                    param.getGReStrictedAreaResponseParam(),0);
            param.setRestrictedArea(restrictedAreaDetail);
            mViewModel.queryLimitResult(param);
        }
    }
}
