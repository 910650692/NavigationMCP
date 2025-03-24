package com.fy.navi.service.adapter.user.behavior.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.user.behavior.BehaviorService;
import com.autonavi.gbl.user.behavior.model.BehaviorServiceParam;
import com.autonavi.gbl.user.behavior.model.ConfigKey;
import com.autonavi.gbl.user.behavior.model.ConfigValue;
import com.autonavi.gbl.user.behavior.model.SimpleFavoriteItem;
import com.autonavi.gbl.user.behavior.observer.IBehaviorServiceObserver;
import com.autonavi.gbl.user.model.UserLoginInfo;
import com.autonavi.gbl.user.syncsdk.SyncSdkService;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.user.syncsdk.model.SyncSdkServiceParam;
import com.autonavi.gbl.user.syncsdk.observer.ISyncSDKServiceObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.behavior.BehaviorAdapterCallBack;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;

import java.util.ArrayList;
import java.util.Hashtable;

public class BehaviorAdapterImplHelper implements IBehaviorServiceObserver, ISyncSDKServiceObserver {
    private static final String TAG = MapDefaultFinalTag.FAVORITE_SERVICE_TAG;
    private final Hashtable<String, BehaviorAdapterCallBack> mAccountResultHashtable;
    private BehaviorService mBehaviorService;
    private SyncSdkService mSyncSdkService;
    private final AccountPackage mAccountPackage;

    protected BehaviorAdapterImplHelper(final BehaviorService behaviorService, final SyncSdkService syncSdkService) {
        mAccountResultHashtable = new Hashtable<>();
        mBehaviorService = behaviorService;
        mSyncSdkService = syncSdkService;
        mAccountPackage = AccountPackage.getInstance();
    }

    protected void initBehaviorService() {
        // 1， 构造 云同步服务初始化参数
        final SyncSdkServiceParam param = new SyncSdkServiceParam();
        param.dataPath = GBLCacheFilePath.SYNC_PATH;// girf_sync.db 文件存储路径
        // 保证传入目录存在
        FileUtils.getInstance().createDir(param.dataPath);
        final int behavior = mSyncSdkService.init(param);
        Logger.i(TAG, "initSyncSdkService: behavior = " + behavior);
        mSyncSdkService.addObserver(this);

        // 2，构造 数据收藏初始化参数
        final BehaviorServiceParam behaviorServiceParam = new BehaviorServiceParam();
        //初始化
        final int res = mBehaviorService.init(behaviorServiceParam);
        Logger.i(TAG, "initBehaviorService: initSetting = " + res);
        // 添加回调观察者（精简收藏夹数据回调）
        mBehaviorService.addObserver(this);

        setLoginInfo();

        setConfig(false);
    }

    /**
     * registerCallBack
     * @param key
     * @param callBack
     */
    public void registerCallBack(final String key, final BehaviorAdapterCallBack callBack) {
        mAccountResultHashtable.put(key, callBack);
    }

    /**
     * unRegisterCallBack
     * @param key
     */
    public void unRegisterCallBack(final String key) {
        mAccountResultHashtable.remove(key);
    }

    /**
     * removeCallback
     */
    public void removeCallback() {
        mAccountResultHashtable.clear();
    }

    /**
     * 同步账号库登录
     */
    public void setLoginInfo() {
        // 设置用户登录信息
        final AccountProfileInfo info = mAccountPackage.getUserInfo();
        final UserLoginInfo userLoginInfo = new UserLoginInfo();
        userLoginInfo.userId = info.uid; // "请填写用户ID";
        //设置用户模式（同步库账号登录）
        final int userRes = mBehaviorService.setLoginInfo(userLoginInfo);
        Logger.i(TAG, "setLoginInfo: userRes = " + userRes);
    }

    /**
     * 设置常去地点（家、公司）开关
     * 650版本之后，收藏点中的家、公司需要增加开关控制，默认不同步到后台。当开关从关到开，将家、公司做一次同步。
     * @param isOpen
     */
    public void setConfig(final boolean isOpen) {
        final ConfigValue value = new ConfigValue();
        //  0:关闭，默认态;  1:开启
        value.intValue = isOpen ? 1 : 0;
        // SyncModeNow同步到后台； SyncModeLater保存到本地
        final int userRes = mBehaviorService.setConfig(ConfigKey.ConfigKeyOftenArrived, value, SyncMode.SyncModeNow);
        Logger.i(TAG, "setConfig: userRes = " + userRes);
    }

    /**
     * 同步常去地点（家、公司）数据
     * 开关开启时会上传本地家、公司到后台，关闭时只会拉取后台数据
     * 注意：HMI需要同步的时候，需要先将ConfigKey.ConfigKeyOftenArrived置为1
     */
    public void syncFrequentData() {
        final int ret = mBehaviorService.syncFrequentData();
        Logger.i(TAG, "syncFrequentData: ret = " + ret);
    }

    /**
     *  同步事件回调通知
     * @param eventType        同步SDK回调事件类型
     * @param exCode           同步SDK返回值
     */
    @Override
    public void notify(final int eventType, final int exCode) {
        Logger.i(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);

        // 收藏业务同步结果回调接口
        // 同步事件处理
      /*  if (ConvertUtils.isEmpty(mAccountResultHashtable)) return;
        for (BehaviorAdapterCallBack callBack : mAccountResultHashtable.values()) {
            if (callBack == null) continue;
            callBack.notifyFavorite(eventType, exCode);
        }*/
    }

    /**
     * 异步获取收藏点回调
     * getFavoriteListAsync的接口回调
     * @param type   收藏点类型
     * @param data   收藏点列表
     * @param sorted 是否排序
     */
    @Override
    public void notify(final int type, final ArrayList<SimpleFavoriteItem> data, final boolean sorted) {
        Logger.i(TAG, "notify: type = " + type + " sorted = " + sorted + " data = " + GsonUtils.toJson(data));
        // 显示 精简收藏夹列表 逻辑
        if (ConvertUtils.isEmpty(mAccountResultHashtable)) {
            return;
        }
        for (BehaviorAdapterCallBack callBack : mAccountResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.notifyFavoriteAsync(type, convertSimpleFavoriteInfo(data), sorted);
        }
    }

    /**
     * convertSimpleFavoriteInfo
     * @param data
     * @return list
     */
    private ArrayList<PoiInfoEntity> convertSimpleFavoriteInfo(final ArrayList<SimpleFavoriteItem> data) {
        if (data == null) {
            return null;
        }
        final ArrayList<PoiInfoEntity> favoriteList = new ArrayList<>();
        for (SimpleFavoriteItem info : data) {
            final FavoriteInfo favoriteInfo = new FavoriteInfo()
                    .setItemId(info.item_id)
                    .setCommonName(info.common_name)
                    .setTag(info.tag)
                    .setType(info.type)
                    .setNewType(info.newType)
                    .setCustom_name(info.custom_name)
                    .setClassification(info.classification)
                    .setTop_time(info.top_time);
            final PoiInfoEntity simpleFavoriteInfo = new PoiInfoEntity()
                    .setPid(String.valueOf(info.id))
                    .setAdCode(Integer.parseInt(info.city_code))
                    .setAddress(info.address)
                    .setPhone(info.phone_numbers)
                    .setPoint(new GeoPoint(info.point_x, info.point_y))
                    .setFavoriteInfo(favoriteInfo);
            favoriteList.add(simpleFavoriteInfo);
        }
        return favoriteList;
    }

    /**
     * 校验账号数据服务状态.
     */
    protected void checkoutBehaviorServer() {
        if (ServiceInitStatus.ServiceNotInit == mBehaviorService.isInit()) {
            initBehaviorService();
        }
    }

}
