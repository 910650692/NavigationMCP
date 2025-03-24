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
     * 获取收藏点id列表（收藏点个数）
     * @return 收藏点个数
     */
    public int[] getSimpleFavoriteIds() {
        return mBehaviorPackage.getSimpleFavoriteIds();
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
        Logger.d("getSimpleFavoriteList = " + mBehaviorPackage.getFavoritePoiData(0).size());
        mViewModel.updateFavoriteView(mBehaviorPackage.getFavoritePoiData(0));
    }

    /**
     * getFavoritePoiData
     */
    public void getFavoritePoiData() {
        mViewModel.updateFavoritePoiData(mBehaviorPackage.getFavoritePoiData(3));
    }

    /**
     * getHomeInfo
     */
    public void getHomeInfo() {
        mViewModel.updateHomeView(getFavoriteHomeData(1));
    }

    /**
     * getCompanyInfo
     */
    public void getCompanyInfo() {
        mViewModel.updateCompanyView(getFavoriteHomeData(2));
    }

    /**
     * getSyncTime
     */
    public void getSyncTime() {
        final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-M-d HH:mm:ss", Locale.getDefault());
        final String formattedDate = dateFormat.format(new Date());
        final String syncTime = mSettingManager.getValueByKey(SettingController.KEY_SETTING_SYNC_TIME);
        if (syncTime == null || syncTime.isEmpty()) {
            mViewModel.updateSyncTime(formattedDate);
        } else {
            mViewModel.updateSyncTime(syncTime);
        }
    }

    @Override
    public void onUpdateSyncTime(final String syncTime) {
        mViewModel.updateSyncTime(syncTime);
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
        final String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED);
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
     * 获取家的信息（sdk）
     * @return entity
     */
    public PoiInfoEntity getHomeFavoriteInfo() {
        return mBehaviorPackage.getHomeFavoriteInfo();
    }

    /**
     * 获取公司的信息（sdk）
     * @return entity
     */
    public PoiInfoEntity getCompanyFavoriteInfo() {
        return mBehaviorPackage.getCompanyFavoriteInfo();
    }

    /**
     * 获取精简收藏点列表（异步）
     * @param type
     * @param sorted
     * @return  int
     */
    public int getFavoriteListAsync(final int type, final boolean sorted) {
        return mBehaviorPackage.getFavoriteListAsync(type, sorted);
    }

    /**
     * 获取收藏点详细信息
     * @param baseInfo
     * @return  entity
     */
    public PoiInfoEntity getFavorite(final PoiInfoEntity baseInfo) {
        return mBehaviorPackage.getFavorite(baseInfo);
    }

    /**
     * 添加POI收藏点
     *
     * @param poiInfo
     * @return string
     */
    public String addFavorite(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.addFavorite(poiInfo);
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
     * 是否是收藏点
     * 当绘制收藏夹图层的时候将item_id调用setid接口设置给图层，点击图层的时候通过getid接口获取itemid赋值给
     * FavoriteBaseItem.item_id来判断是否收藏，避免通过转换精度丢失导致判断出错问题
     * @param poiInfo
     * @return boolean
     */
    public boolean isFavorite(final PoiInfoEntity poiInfo) {
        return !mBehaviorPackage.isFavorite(poiInfo).isEmpty();
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

    @Override
    public void notifyFavorite(final int eventType, final int exCode) {
        mViewModel.updateFavoriteView(mBehaviorPackage.getSimpleFavoriteList());
    }

    @Override
    public void notifyFavoriteAsync(final int type, final ArrayList<PoiInfoEntity> data, final boolean sorted) {
        mViewModel.updateFavoriteView(mBehaviorPackage.getSimpleFavoriteList());
    }

    /**
     * 添加收藏夹信息到本地数据库
     * @param entity
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void addFavoriteData(final PoiInfoEntity entity, final int favoriteType) {
        mBehaviorPackage.addFavoriteData(entity, favoriteType);
    }

    /**
     * 在本地获取家/公司的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return entity
     */
    public PoiInfoEntity getFavoriteHomeData(final int favoriteType) {
        return mBehaviorPackage.getFavoriteHomeData(favoriteType);
    }

    /**
     * 在本地获取常去地址/普通收藏点的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return list
     */
    public ArrayList<PoiInfoEntity> getFavoritePoiData(final int favoriteType) {
        return mBehaviorPackage.getFavoritePoiData(favoriteType);
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

    /**
     * 删除 itemId 对应的本地数据
     * @param itemId  收藏点唯一码
     */
    public void deleteFavoriteData(final String itemId) {
        mBehaviorPackage.deleteFavoriteData(itemId);
    }

}

