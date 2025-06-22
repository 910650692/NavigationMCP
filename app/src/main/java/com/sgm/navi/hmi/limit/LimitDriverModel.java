package com.sgm.navi.hmi.limit;


import android.os.Handler;
import android.os.Looper;

import com.android.utils.log.Logger;
import com.sgm.navi.service.define.aos.RestrictedArea;
import com.sgm.navi.service.define.aos.RestrictedParam;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteRestrictionParam;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.sgm.navi.service.logicpaket.aos.IAosRestrictedObserver;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.UUID;

/**
 * @author QiuYaWei
 * @version  \$Revision.1.0\$
 * Date: 2025/2/7
 * Description: [TODO]
 */
public class LimitDriverModel extends BaseModel<LimitDriverViewModel> implements IAosRestrictedObserver {
    private static final String TAG = "LimitDriverModel";
    private static Handler mHandler;
    private Long mLimitQueryTaskId;
    private String mCurrentCityCode;
    private final String mCallbackId;
    private static final int FAIL_TIME = 10 * 1000;
    private final Runnable mLoadingFail = new Runnable() {
        @Override
        public void run() {
            mLimitQueryTaskId = null;
            mViewModel.loadingFail();
        }
    };

    public LimitDriverModel() {
        mCallbackId = UUID.randomUUID().toString();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mHandler = new Handler(Looper.getMainLooper());
        AosRestrictedPackage.getInstance().addRestrictedObserver(mCallbackId, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        AosRestrictedPackage.getInstance().removeRestrictedObserver(mCallbackId);
    }

    /**
     * 通过城市id请求限行政策
     * @param cityCode 城市id
     */
    public void queryLimitPolicyByCityCode(final String cityCode) {
        Logger.d(TAG, "queryRetry cityCode:" , cityCode);
        final String license = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        if (license == null || license.isEmpty()) {
            Logger.d(TAG, "No license plate set");
            return;
        }
        mCurrentCityCode = cityCode;
        final RestrictedParam restrictedParam = new RestrictedParam();
        // 请求根据车型等信息获取的规则 type = 7 请求城市全部规则 type = 8 请求城市列表 type = 9 根据规则请求数据
        restrictedParam.setRestrict_type(7);
        restrictedParam.setPlate(license);
        restrictedParam.setAdcodes(cityCode);
        mLimitQueryTaskId = AosRestrictedPackage.getInstance().queryRestrictedInfo(restrictedParam);
        mHandler.postDelayed(mLoadingFail, FAIL_TIME);
    }

    /**
     * 绘制限行区域
     *
     * @param routeRestrictionParam 限行政策
     */
    public void drawRestrictionForLimit(final RouteRestrictionParam routeRestrictionParam) {
        RoutePackage.getInstance().drawRestrictionForLimit(MapType.MAIN_SCREEN_MAIN_MAP,
                routeRestrictionParam.getMReStrictedAreaResponseParam(), 0);
        AosRestrictedPackage.getInstance().showRestrictedAreaPreview(MapType.MAIN_SCREEN_MAIN_MAP,
                routeRestrictionParam, 0);
    }

    /**
     * 请求限行信息重试
     */
    public void queryRetry() {
        if (mCurrentCityCode != null && !mCurrentCityCode.isEmpty()) {
            Logger.d(TAG, "queryRetry cityCode:" , mCurrentCityCode);
            queryLimitPolicyByCityCode(mCurrentCityCode);
        }
    }

    @Override
    public void queryLimitResult(final RouteRestrictionParam param) {
        // 限行信息查询成功后更新UI
        if (mLimitQueryTaskId == null) {
            return;
        }
        final RestrictedArea restrictedAreaDetail = param.getMRestrictedArea();
        if (mLimitQueryTaskId == restrictedAreaDetail.getMRequestId()) {
            mHandler.removeCallbacks(mLoadingFail);
            if (restrictedAreaDetail.getMCityNames() == null || restrictedAreaDetail.getMCityNames().isEmpty()) {
                if (mCurrentCityCode != null && !mCurrentCityCode.isEmpty()) {
                    final ArrayList<String> cityName = new ArrayList<>();
                    cityName.add(MapDataPackage.getInstance()
                            .getCityInfo(Integer.parseInt(mCurrentCityCode)).getName());
                    restrictedAreaDetail.setMCityNames(cityName);
                }
            }

            RoutePackage.getInstance().drawRestrictionForLimit(MapType.MAIN_SCREEN_MAIN_MAP,
                    param.getMReStrictedAreaResponseParam(),0);
            param.setMRestrictedArea(restrictedAreaDetail);
            AosRestrictedPackage.getInstance().showRestrictedAreaPreview(MapType.MAIN_SCREEN_MAIN_MAP, param, 0);
            if (mViewModel != null) {
                mViewModel.showPolicyUI(param);
            }
        }
    }
}
