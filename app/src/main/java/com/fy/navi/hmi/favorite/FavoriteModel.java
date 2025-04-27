package com.fy.navi.hmi.favorite;

import com.alibaba.android.arouter.utils.TextUtils;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorCallBack;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;

public class FavoriteModel extends BaseModel<FavoriteViewModel> implements BehaviorCallBack, SettingUpdateObservable.SettingUpdateObserver {
    private static final String TAG = FavoriteModel.class.getName();
    private final BehaviorPackage mBehaviorPackage;
    private final SettingManager mSettingManager;

    public FavoriteModel() {
        mBehaviorPackage = BehaviorPackage.getInstance();
        mSettingManager = new SettingManager();
        mSettingManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mBehaviorPackage.registerCallBack(this);
        SettingUpdateObservable.getInstance().addObserver("FavoriteModel", this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 手动同步
     */
    public void startSync() {
        mBehaviorPackage.startSync();
    }

    /**
     * 获取精简收藏点列表（普通poi）
     */
    public void getSimpleFavoriteList() {
        mViewModel.updateFavoriteView(mBehaviorPackage.getFavoritePoiData());
    }

    /**
     * getHomeInfo
     */
    public void getHomeFavoriteInfo() {
        mViewModel.updateHomeView(mBehaviorPackage.getHomeFavoriteInfo());
    }

    /**
     * getCompanyInfo
     */
    public void getCompanyFavoriteInfo() {
        mViewModel.updateCompanyView(mBehaviorPackage.getCompanyFavoriteInfo());
    }

    /**
     * getCompanyInfo
     */
    public ArrayList<PoiInfoEntity> getFavoriteAddressInfo() {
       return mBehaviorPackage.getFavoriteAddressInfo();
    }


    @Override
    public void onUpdateSyncTime(final String syncTime) {
//        mViewModel.updateSyncTime(syncTime);
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_SYNC_TIME, syncTime);
    }

    /**
     * setHomeCompanyDisplay
     * @param isHomeCompanyDisplay
     */
    public void setHomeCompanyDisplay(final boolean isHomeCompanyDisplay) {
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, String.valueOf(isHomeCompanyDisplay));
    }

    /**
     * initView
     */
    public void initView() {
        getHomeCompanyDisplay();
        getCarType();
    }

    /**
     * getHomeCompanyDisplay
     */
    public void getHomeCompanyDisplay() {
        String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
        }
        final boolean isHomeCompanyDisplay = Boolean.parseBoolean(value);
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, isHomeCompanyDisplay);
    }

    /**
     * getCarType
     */
    public void getCarType() {
        final int carMode = CalibrationPackage.getInstance().powerType();
        final boolean isEVCar = carMode == 1;
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_IS_EV_CAR, isEVCar);
    }

    /**
     * 删除POI收藏点
     * @param poiInfo
     * @return string
     */
    public String removeFavorite(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.removeFavorite(poiInfo);
    }

    /**
     * 收藏点置顶/取消置顶
     * @param info
     * @param isSetTop
     * @return string
     */
    public String topFavorite(final PoiInfoEntity info, final boolean isSetTop) {
        return mBehaviorPackage.topFavorite(info, isSetTop);
    }

    /**
     * 收藏点重命名
     * @param detailInfo
     * @param customName
     * @return string
     */
    public String modifyFavorite(final PoiInfoEntity detailInfo, final String customName) {
        return mBehaviorPackage.modifyFavorite(detailInfo, customName);
    }

    /**
     * // 用户处于登录状态，完成同步事件 获取指定Uid的精简信息收藏点列表
     * @param eventType
     * @param exCode
     */
    @Override
    public void notifyFavorite(final int eventType, final int exCode) {
        getSimpleFavoriteList();
    }

    @Override
    public void notifyFavoriteAsync(final int type, final ArrayList<PoiInfoEntity> data, final boolean sorted) {
        mViewModel.updateFavoriteView(data);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param customName  自定义名称 重命名时编辑的字段
     */
    public void modifyFavoriteData(final String itemId, final String customName) {
        mBehaviorPackage.modifyFavoriteData(itemId, customName);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param topTime 置顶时间
     */
    public void updateFavoriteTopTime(final String itemId, final long topTime) {
        mBehaviorPackage.updateFavoriteTopTime(itemId, topTime);
    }

}

