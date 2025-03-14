package com.fy.navi.hmi.favorite;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorCallBack;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseModel;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/23
 */
public class FavoriteModel extends BaseModel<FavoriteViewModel> implements BehaviorCallBack, SettingUpdateObservable.SettingUpdateObserver {
    private static final String TAG = FavoriteModel.class.getName();
    private final BehaviorPackage behaviorPackage;
    private final SettingManager settingManager;


    public FavoriteModel() {
        behaviorPackage = BehaviorPackage.getInstance();
        settingManager = new SettingManager();
        settingManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        behaviorPackage.registerCallBack(this);
        SettingUpdateObservable.getInstance().addObserver("FavoriteModel", this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 获取收藏点id列表（收藏点个数）
     */
    public int[] getSimpleFavoriteIds() {
        return behaviorPackage.getSimpleFavoriteIds();
    }

    /**
     * 手动同步
     */
    public void startSync() {
        behaviorPackage.startSync();
    }

    /**
     * 获取精简收藏点列表（普通poi）
     */
    public void getSimpleFavoriteList() {
        Logger.d("getSimpleFavoriteList = " + behaviorPackage.getFavoritePoiData(0).size());
        mViewModel.updateFavoriteView(behaviorPackage.getFavoritePoiData(0));
    }

    public void getFavoritePoiData() {
        mViewModel.updateFavoritePoiData(behaviorPackage.getFavoritePoiData(3));
    }

    public void getHomeInfo() {
        mViewModel.updateHomeView(getFavoriteHomeData(1));
    }

    public void getCompanyInfo() {
        mViewModel.updateCompanyView(getFavoriteHomeData(2));
    }

    public void getSyncTime() {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-M-d HH:mm:ss", Locale.getDefault());
        String formattedDate = dateFormat.format(new Date());
        String syncTime = settingManager.getValueByKey(SettingController.KEY_SETTING_SYNC_TIME);
        if (syncTime == null || syncTime.isEmpty()) {
            mViewModel.updateSyncTime(formattedDate);
        } else {
            mViewModel.updateSyncTime(syncTime);
        }
    }

    @Override
    public void onUpdateSyncTime(String syncTime) {
        mViewModel.updateSyncTime(syncTime);
        settingManager.insertOrReplace(SettingController.KEY_SETTING_SYNC_TIME, syncTime);
    }

    public void setHomeCompanyDisplay(boolean isHomeCompanyDisplay) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, String.valueOf(isHomeCompanyDisplay));
    }

    public void initView() {
        getHomeCompanyDisplay();
        getCarType();
    }

    public void getHomeCompanyDisplay() {
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED);
        boolean isHomeCompanyDisplay = Boolean.parseBoolean(value);
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, isHomeCompanyDisplay);
    }

    public void getCarType() {
        int carMode = CalibrationPackage.getInstance().powerType();
        boolean isEVCar = carMode == 1;
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_IS_EV_CAR, isEVCar);
    }

    /**
     * 获取家的信息（sdk）
     * @return
     */
    public PoiInfoEntity getHomeFavoriteInfo() {
        return behaviorPackage.getHomeFavoriteInfo();
    }

    /**
     * 获取公司的信息（sdk）
     * @return
     */
    public PoiInfoEntity getCompanyFavoriteInfo() {
        return behaviorPackage.getCompanyFavoriteInfo();
    }

    /**
     * 获取精简收藏点列表（异步）
     */
    public int getFavoriteListAsync(int type, boolean sorted) {
        return behaviorPackage.getFavoriteListAsync(type, sorted);
    }

    /**
     * 获取收藏点详细信息
     */
    public PoiInfoEntity getFavorite(PoiInfoEntity baseInfo) {
        return behaviorPackage.getFavorite(baseInfo);
    }

    /**
     * 添加POI收藏点
     *
     * @param poiInfo
     * @return
     */
    public String addFavorite(PoiInfoEntity poiInfo) {
        return behaviorPackage.addFavorite(poiInfo);
    }

    /**
     * 删除POI收藏点
     */
    public String removeFavorite(PoiInfoEntity poiInfo) {
        return behaviorPackage.removeFavorite(poiInfo);
    }

    /**
     * 是否是收藏点
     * 当绘制收藏夹图层的时候将item_id调用setid接口设置给图层，点击图层的时候通过getid接口获取itemid赋值给
     * FavoriteBaseItem.item_id来判断是否收藏，避免通过转换精度丢失导致判断出错问题
     */
    public boolean isFavorite(PoiInfoEntity poiInfo) {
        return !behaviorPackage.isFavorite(poiInfo).isEmpty();
    }

    /**
     * 收藏点置顶/取消置顶
     */
    public String topFavorite(PoiInfoEntity info, boolean bSetTop) {
        return behaviorPackage.topFavorite(info, bSetTop);
    }

    /**
     * 收藏点重命名
     */
    public String modifyFavorite(PoiInfoEntity detailInfo, String customName) {
        return behaviorPackage.modifyFavorite(detailInfo, customName);
    }

    @Override
    public void notifyFavorite(int eventType, int exCode) {
        mViewModel.updateFavoriteView(behaviorPackage.getSimpleFavoriteList());
    }

    @Override
    public void notifyFavoriteAsync(int type, ArrayList<PoiInfoEntity> data, boolean sorted) {
        mViewModel.updateFavoriteView(behaviorPackage.getSimpleFavoriteList());
    }

    /**
     * 添加收藏夹信息到本地数据库
     * @param entity
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void addFavoriteData(PoiInfoEntity entity, int favoriteType) {
        behaviorPackage.addFavoriteData(entity, favoriteType);
    }

    /**
     * 在本地获取家/公司的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return
     */
    public PoiInfoEntity getFavoriteHomeData(int favoriteType) {
        return behaviorPackage.getFavoriteHomeData(favoriteType);
    }

    /**
     * 在本地获取常去地址/普通收藏点的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return
     */
    public ArrayList<PoiInfoEntity> getFavoritePoiData(int favoriteType) {
        return behaviorPackage.getFavoritePoiData(favoriteType);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param customName  自定义名称 重命名时编辑的字段
     */
    public void modifyFavoriteData(String itemId, String customName) {
        behaviorPackage.modifyFavoriteData(itemId, customName);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param topTime 置顶时间
     */
    public void updateFavoriteTopTime(String itemId, long topTime) {
        behaviorPackage.updateFavoriteTopTime(itemId, topTime);
    }

    /**
     * 删除 itemId 对应的本地数据
     * @param itemId  收藏点唯一码
     */
    public void deleteFavoriteData(String itemId) {
        behaviorPackage.deleteFavoriteData(itemId);
    }

}

