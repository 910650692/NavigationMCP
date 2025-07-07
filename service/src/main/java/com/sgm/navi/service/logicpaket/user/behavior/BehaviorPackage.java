package com.sgm.navi.service.logicpaket.user.behavior;

import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.R;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.user.behavior.BehaviorAdapter;
import com.sgm.navi.service.adapter.user.behavior.BehaviorAdapterCallBack;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.account.AccountUserInfo;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.greendao.favorite.Favorite;
import com.sgm.navi.service.greendao.favorite.FavoriteManager;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountCallBack;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

final public class BehaviorPackage implements BehaviorAdapterCallBack, AccountCallBack {
    public static final String TAG = BehaviorPackage.class.getName();
    private BehaviorAdapter mBehaviorAdapter;
    private final List<BehaviorCallBack> mCallBacks = new ArrayList<>();
    private final FavoriteManager mManager;
    private final CommonManager mCommonManager;
    private MapAdapter mapAdapter;
    private LayerAdapter mLayerAdapter;
    private final List<FavoriteStatusCallback> mFavoriteStatusCallbacks = new ArrayList<>();
    private final SettingManager mSettingManager;
    private boolean mIsFavoriteDataUpdated = true;      //设置开关关闭时是否有收藏信息变更；

    private BehaviorPackage() {
        mManager = FavoriteManager.getInstance();
        mManager.init();
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
        mSettingManager = new SettingManager();
        mSettingManager.init();
    }

    /**
     * initBehaviorService
     */
    public void initBehaviorService() {
        mBehaviorAdapter = BehaviorAdapter.getInstance();
        mapAdapter = MapAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mBehaviorAdapter.initBehaviorService();
        mBehaviorAdapter.registerCallBack("BehaviorPackage", this);
        AccountPackage.getInstance().registerCallBack("BehaviorPackage", this);
    }

    /**
     * 设置账号信息
     */
    public void setLoginInfo() {
        mBehaviorAdapter.setLoginInfo();
    }

    /**
     * registerCallBack
     * @param callback
     */
    public synchronized void registerCallBack(final BehaviorCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.add(callback);
        }
    }

    public synchronized void registerFavoriteStatusCallback(final FavoriteStatusCallback callback) {
        if (callback != null && !mFavoriteStatusCallbacks.contains(callback)) {
            mFavoriteStatusCallbacks.add(callback);
        }
    }

    public void unRegisterCallBack(final BehaviorCallBack callback) {
        if (callback == null) {
            Logger.e(TAG,"unRegisterCallBack callback == null");
            return;
        }
        if (mCallBacks.contains(callback)){
            mCallBacks.remove(callback);
        }
    }

    public synchronized void unRegisterFavoriteStatusCallback(final FavoriteStatusCallback callback) {
        if (callback == null) {
            Logger.e(TAG,"unRegisterFavoriteStatusCallback callback == null");
            return;
        }
        if (mFavoriteStatusCallbacks.contains(callback)){
            mFavoriteStatusCallbacks.remove(callback);
        }
    }

    /**
     * 判断用户是否已登录
     * @return 返回登录状态
     */
    private boolean isLogin() {
        boolean isLogin = false;
        final String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i(TAG,"getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            final AccountProfileInfo  info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            isLogin = info != null && !TextUtils.isEmpty(info.getUid());
        }
       return isLogin;
    }

    /**
     * 获取收藏点信息（普通POI点）
     * 用户未登录，从本地获取数据；用户已登录，从云端同步数据
     * @return list
     */
    public ArrayList<PoiInfoEntity> getFavoritePoiData() {
        ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        if (isLogin()) { // 用户已登录，从云端获取数据
            dataList = mBehaviorAdapter.getSimpleFavoriteList();
            if (dataList != null && dataList.size() > 0) {
                Logger.d(TAG, "getFavoritePoiData: size = " + dataList.size());
            }
        } else { // 用户未登录，从本地获取数据
            final List<Favorite> favoriteList = mManager.getValueByCommonName(UserDataCode.FAVORITE_TYPE_POI);
            if (!ConvertUtils.isEmpty(favoriteList)) {
                for (Favorite item : favoriteList) {
                    final PoiInfoEntity simpleFavoriteInfo = getPoiInfoEntity(item);
                    dataList.add(simpleFavoriteInfo);
                }
            }
            dataList.sort((poiInfoEntity1, poiInfoEntity2) -> {
                final boolean isTopFirst = poiInfoEntity1.getFavoriteInfo().getTop_time() != 0;
                final boolean isTopSecond = poiInfoEntity2.getFavoriteInfo().getTop_time() != 0;

                // 第一优先级：置顶状态
                if (isTopFirst != isTopSecond) {
                    return isTopFirst ? -1 : 1; // 置顶项始终在前
                }

                // 第二优先级（仅当两者都置顶时）：top_time倒序
                if (isTopFirst) {
                    final long topDiff = poiInfoEntity2.getFavoriteInfo().getTop_time() - poiInfoEntity1.getFavoriteInfo().getTop_time();
                    if (topDiff != 0) {
                        return Long.compare(poiInfoEntity2.getFavoriteInfo().getTop_time(), poiInfoEntity1.getFavoriteInfo().getTop_time());
                    }
                }

                // 第三优先级：updateTime倒序
                return Long.compare(poiInfoEntity2.getFavoriteInfo().getUpdateTime(), poiInfoEntity1.getFavoriteInfo().getUpdateTime());
            });
        }

        Logger.d(TAG, "favorite list is " + GsonUtils.toJson(dataList));
        return dataList;
    }

    /**
     * 获取家的数据
     * 用户未登录，从本地获取数据；用户已登录，从云端同步数据
     * @return entity
     */
    public PoiInfoEntity getHomeFavoriteInfo() {
        PoiInfoEntity poiInfoEntity = null;
        if (isLogin()) { // 用户已登录，从云端获取数据
            if (mBehaviorAdapter != null) {
                poiInfoEntity = mBehaviorAdapter.getHomeFavoriteInfo();
            }
        } else { // 用户未登录，从本地获取数据
            final List<Favorite> favoriteList = mManager.getValueByCommonName(UserDataCode.FAVORITE_TYPE_HOME);
            if (!ConvertUtils.isEmpty(favoriteList)) {
                poiInfoEntity = getPoiInfoEntity(favoriteList.get(0));
            }
        }
        return poiInfoEntity;
    }

    /**
     * 获取公司的数据
     * 用户未登录，从本地获取数据；用户已登录，从云端同步数据
     * @return  entity
     */
    public PoiInfoEntity getCompanyFavoriteInfo() {
        PoiInfoEntity poiInfoEntity = null;
        if (isLogin()) { // 用户已登录，从云端获取数据
            if (mBehaviorAdapter != null) {
                poiInfoEntity = mBehaviorAdapter.getCompanyFavoriteInfo();
            }
        } else { // 用户未登录，从本地获取数据
            final List<Favorite> favoriteList = mManager.getValueByCommonName(UserDataCode.FAVORITE_TYPE_COMPANY);
            if (!ConvertUtils.isEmpty(favoriteList)) {
                poiInfoEntity = getPoiInfoEntity(favoriteList.get(0));
            }
        }
        return poiInfoEntity;
    }

    /**
     * 获取常去地址信息
     * 仅在本地数据库进行管理
     * @return list
     */
    public ArrayList<PoiInfoEntity> getFavoriteAddressInfo() {
        final ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        final List<Favorite> favoriteList = mManager.getValueByCommonName(UserDataCode.FAVORITE_TYPE_ADDRESS);
        if (!ConvertUtils.isEmpty(favoriteList)) {
            for (Favorite item : favoriteList) {
                final PoiInfoEntity simpleFavoriteInfo = getPoiInfoEntity(item);
                dataList.add(simpleFavoriteInfo);
            }
        }
        return dataList;
    }

    /**
     * getFavorite
     * @param baseInfo
     * @return entity
     */
    public PoiInfoEntity getFavorite(final PoiInfoEntity baseInfo) {
        return mBehaviorAdapter.getFavorite(baseInfo);
    }

    /**
     * 添加收藏点
     * 注意区分收藏点类型  common_name：1，家  2，公司  0，普通收藏点
     * @param type 收藏点类型
     * @param poiInfo 收藏点信息
     * @return string
     */
    public String addFavorite(final PoiInfoEntity poiInfo, final int type) {
        String itemId = "";
        if (poiInfo == null) {
            Logger.e(TAG, "the poi info is null");
            return itemId;
        }
        Logger.d(TAG, "add favorite " + GsonUtils.toJson(poiInfo));
        PoiInfoEntity savedPoiInfo = null;
        if (type == 1) {
            savedPoiInfo = getHomeFavoriteInfo();
        } else if (type == 2) {
            savedPoiInfo = getCompanyFavoriteInfo();
        }

        if (isLogin() && type != 3) {
            itemId =  mBehaviorAdapter.addFavorite(poiInfo);
        } else {
            if (poiInfo.getFavoriteInfo() == null) {
                Logger.e(TAG, "the favorite info is null");
                return itemId;
            }
            if (TextUtils.isEmpty(poiInfo.getFavoriteInfo().getItemId())) {
                itemId = poiInfo.getPid();
            } else {
                itemId = poiInfo.getFavoriteInfo().getItemId();
            }
            Logger.d(TAG, "add fav itemId is: " + itemId);
            poiInfo.getFavoriteInfo().setItemId(itemId);
            addFavoriteData(poiInfo, type);
        }
        if (type == 1 || type == 2) {
            notifyFavoriteStatusChanged();
        }

        //更新家或公司时成功时需要把原先的扎点去掉
        if (savedPoiInfo != null && !TextUtils.isEmpty(itemId)) {
            updateFavoriteMain(savedPoiInfo, false);
        }

        //非常去地址添加成功后更新扎点
        if (type != 3 && !TextUtils.isEmpty(itemId)) {
            poiInfo.getFavoriteInfo().setItemId(itemId);
            updateFavoriteMain(poiInfo, true);
        }
        Logger.d(TAG, "add favorite success " + itemId);
        return itemId;
    }

    /**
     * 移除收藏点
     *
     * @param poiInfo
     * @return item id
     */
    public String removeFavorite(final PoiInfoEntity poiInfo) {
        String itemId = "";
        if (poiInfo == null || poiInfo.getFavoriteInfo() == null) {
            Logger.e(TAG, "poi info or favorite info is null");
            return itemId;
        }
        //For Bury Point
        sendBuryPointForRemovingOldFavorite(poiInfo);
        if (isLogin() && poiInfo.getFavoriteInfo().getCommonName() != 3) {
            itemId =  mBehaviorAdapter.removeFavorite(poiInfo);
        } else {
            itemId = poiInfo.getFavoriteInfo().getItemId();
            deleteFavoriteData(itemId);
        }
        Logger.d(TAG, "remove favorite " + itemId);
        //删除成功
        if (!TextUtils.isEmpty(itemId)) {
            poiInfo.getFavoriteInfo().setItemId(itemId);
            updateFavoriteMain(poiInfo, false);
        }
        notifyFavoriteStatusChanged();
        return itemId;
    }

    private void notifyFavoriteStatusChanged() {
        for (FavoriteStatusCallback callback : mFavoriteStatusCallbacks) {
            if (callback != null) {
                callback.notifyFavoriteHomeChanged(getHomeFavoriteInfo() != null);
                callback.notifyFavoriteCompanyChanged(getCompanyFavoriteInfo() != null);
            }
        }
    }

    @HookMethod
    private void sendBuryPointForRemovingOldFavorite(final PoiInfoEntity poiInfo) {
        if (poiInfo != null && poiInfo.getFavoriteInfo() != null) {
            final String eventName = switch (poiInfo.getFavoriteInfo().getCommonName()){
                case 0 -> BuryConstant.EventName.AMAP_FAVORITE_UNSAVE;
                case 1 -> BuryConstant.EventName.AMAP_HOME_UNSAVE;
                case 2 -> BuryConstant.EventName.AMAP_WORK_UNSAVE;
                default -> BuryConstant.EventName.AMAP_UNKNOWN;
            };
            BuryPointController.getInstance().setEventName(eventName);
            final BuryProperty buryProperty = new BuryProperty.Builder()
                    .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, poiInfo.getName())
                    .build();
            BuryPointController.getInstance().setBuryProps(buryProperty);
        }
    }

    /**
     * 是否已收藏
     *
     * @param poiInfo
     * @return string
     */
    public String isFavorite(final PoiInfoEntity poiInfo) {
        String itemId = "";
        if (poiInfo == null) {
            Logger.e(TAG, "poi info is null");
            return itemId;
        }
        if (isLogin()) {
            itemId = mBehaviorAdapter.isFavorite(poiInfo);
        } else {
            if (TextUtils.isEmpty(poiInfo.getPid())) {
                Logger.e(TAG, "the pid is empty");
                return itemId;
            }
            if (poiInfo.getFavoriteInfo() == null || TextUtils.isEmpty(poiInfo.getFavoriteInfo().getItemId())) {
                itemId = poiInfo.getPid();
            } else {
                itemId = poiInfo.getFavoriteInfo().getItemId();
            }
            //普通收藏点
            itemId = isFavorite(itemId) ? itemId : "";
        }
        Logger.d(TAG, "is favorite " + itemId);
        return itemId;
    }

    /**
     * 判断是否是常去地址
     * @param info PoiInfoEntity
     * @return  boolean
     */
    public boolean isFrequentAddress(final PoiInfoEntity info) {
        if (info == null) {
            Logger.e(TAG, "info is null");
            return false;
        }
        final ArrayList<PoiInfoEntity> list = getFavoriteAddressInfo();
        if (list.isEmpty()) {
            return false;
        }

        for (PoiInfoEntity poiInfo : list) {
            if (TextUtils.equals(poiInfo.getPid(), info.getPid())) {
                return true;
            }
        }
        return false;
    }

    /**
     * topFavorite
     * @param baseInfo
     * @param isSetTop
     * @return string
     */
    public String topFavorite(final PoiInfoEntity baseInfo, final boolean isSetTop) {
        if (isLogin()) {
            return mBehaviorAdapter.topFavorite(baseInfo, isSetTop);
        }
        updateFavoriteTopTime(baseInfo.getFavoriteInfo().getItemId(), isSetTop ? System.currentTimeMillis() : 0);
        return "top favorite";
    }

    /**
     * modifyFavorite
     * @param detailInfo
     * @param customName
     * @return string
     */
    public String modifyFavorite(final PoiInfoEntity detailInfo, final String customName) {
        return mBehaviorAdapter.modifyFavorite(detailInfo, customName);
    }

    /**
     * startSync
     */
    public void startSync() {
        mBehaviorAdapter.startSync();
    }

    @Override
    public void notifyFavorite(final int eventType, final int exCode) {
        for (BehaviorCallBack observer : mCallBacks) {
            observer.notifyFavorite(eventType, exCode);
        }
    }

    /**
     * 异步获取收藏点回调
     *
     * @param type   收藏点类型
     * @param data   收藏点列表
     * @param sorted 是否排序
     */
    @Override
    public void notifyFavoriteAsync(final int type, final ArrayList<PoiInfoEntity> data, final boolean sorted) {
        for (BehaviorCallBack observer : mCallBacks) {
            observer.notifyFavoriteAsync(type, data, sorted);
        }
    }

    /**
     * 添加收藏夹信息到本地数据库
     *
     * @param entity
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void addFavoriteData(final PoiInfoEntity entity, final int favoriteType) {
        if (favoriteType == 3) {
            final ArrayList<PoiInfoEntity> commonList = getFavoriteAddressInfo();
            if (commonList.size() >= 3) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        AppCache.getInstance().getMContext().getString(R.string.st_maximum_added_common_address));
                return;
            }
        }

        final Favorite favorite = new Favorite();
        favorite.setMItemId(entity.getFavoriteInfo().getItemId());
        favorite.setMCommonName(favoriteType);
        favorite.setMCustomName(entity.getFavoriteInfo().getCustom_name());
        favorite.setMPid(entity.getPid());
        favorite.setMName(entity.getName());
        favorite.setMPhone(entity.getPhone());
        favorite.setMDistance(formatDistance(entity.getDistance()));
//        favorite.cityName = entity.getCityInfo().getCityName();
//        favorite.cityCode = String.valueOf(entity.getCityInfo().getCityCode());
        favorite.setMAddress(entity.getAddress());
        favorite.setMPointX(entity.getPoint().getLon());
        favorite.setMPointY(entity.getPoint().getLat());
//        favorite.point_x_arrive = entity.arrivePoint;
//        favorite.point_y_arrive = entity.arrivePoint;
        favorite.setMUpdateTime(new Date());
        //家和公司只有一个地址，添加前先清除之前的数据再添加
        if (favoriteType == 1 || favoriteType == 2) {
            deleteFavoriteDataByType(favoriteType);
        }
        mManager.insertValue(favorite);
    }

    /**
     * 转换距离单位 米-->m, 公里-->km
     * @param distance 距离
     * @return 转换后的距离
     */
    private String formatDistance(final String distance) {
        String dis = "";
        if (distance == null || TextUtils.isEmpty(distance)) {
            return dis;
        }
        if (distance.endsWith("米")) {
            dis = distance.substring(0, distance.length() - 1) + "m";
        } else if (distance.endsWith("公里")) {
            dis = distance.substring(0, distance.length() - 2) + "km";
        }
        return dis;
    }

    /**
     * 在本地获取家/公司的信息
     *
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return entity
     */
//    public PoiInfoEntity getFavoriteHomesData(final int favoriteType) {
//        final List<Favorite> favoriteList = mManager.getValueByCommonName(favoriteType);
//        if (null != favoriteList && !favoriteList.isEmpty()) {
//            return getPoiInfoEntity(favoriteList.get(0));
//        }
//        return null;
//    }

    /**
     * 获取未置顶列表
     *
     * @return 未置顶列表
     */
    public ArrayList<PoiInfoEntity> getFavoriteNotTop() {
        final List<Favorite> favoriteList = mManager.getFavoriteNotTop();
        final ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        if (favoriteList != null && !favoriteList.isEmpty()) {
            for (Favorite item : favoriteList) {
                final PoiInfoEntity simpleFavoriteInfo = getPoiInfoEntity(item);
                dataList.add(simpleFavoriteInfo);
            }
        }
        dataList.sort(Comparator.comparingLong(poiInfoEntity -> poiInfoEntity.getFavoriteInfo().getUpdateTime()));
        return dataList;
    }

    /**
     * 在本地获取常已置顶的信息
     *
     * @return 已置顶列表
     */
    public ArrayList<PoiInfoEntity> getValueByTopTime() {
        final List<Favorite> favoriteList = mManager.getValueByTopTime();
        final ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        if (favoriteList != null && !favoriteList.isEmpty()) {
            for (Favorite item : favoriteList) {
                final PoiInfoEntity simpleFavoriteInfo = getPoiInfoEntity(item);
                dataList.add(simpleFavoriteInfo);
            }
        }
        dataList.sort((poiInfoEntity1, poiInfoEntity2) -> {
            final long topDiff = poiInfoEntity2.getFavoriteInfo().getTop_time() - poiInfoEntity1.getFavoriteInfo().getTop_time();
            if (topDiff != 0) {
                return Long.compare(poiInfoEntity2.getFavoriteInfo().getTop_time(), poiInfoEntity1.getFavoriteInfo().getTop_time());
            }
            return Long.compare(poiInfoEntity2.getFavoriteInfo().getUpdateTime(), poiInfoEntity1.getFavoriteInfo().getUpdateTime());
        });
        return dataList;
    }

    /**
     * getPoiInfoEntity
     * @param item item
     * @return entity
     */
    public PoiInfoEntity getPoiInfoEntity(final Favorite item) {
        final FavoriteInfo info = new FavoriteInfo()
                .setItemId(item.getMItemId())
                .setCommonName(item.getMCommonName())
                .setTag(item.getMTag())
                .setType(item.getMType())
                .setNewType(item.getMNewType())
                .setCustom_name(item.getMCustomName())
                .setClassification(item.getMClassification())
                .setUpdateTime(item.getMUpdateTime().getTime())
                .setTop_time(item.getMTopTime());

        final PoiInfoEntity simpleFavoriteInfo = new PoiInfoEntity()
                .setPid(String.valueOf(item.getMPid()))
                .setAddress(item.getMAddress())
                .setName(item.getMName())
                .setPhone(item.getMPhone())
                .setDistance(item.getMDistance())
                .setPoint(new GeoPoint(item.getMPointX(), item.getMPointY()))
                .setFavoriteInfo(info);
        return simpleFavoriteInfo;
    }

    /**
     * 重命名时需更新本地数据
     *
     * @param itemId     收藏点唯一码
     * @param customName 自定义名称 重命名时编辑的字段
     */
    public void modifyFavoriteData(final String itemId, final String customName) {
        mManager.updateCustomName(itemId, customName);
    }

    /**
     * 重命名时需更新本地数据
     *
     * @param itemId  收藏点唯一码
     * @param topTime 置顶时间
     */
    public void updateFavoriteTopTime(final String itemId, final long topTime) {
        mManager.updateTopTime(itemId, topTime);
    }

    /**
     * 删除 itemId 对应的本地数据
     *
     * @param itemId 收藏点唯一码
     */
    private void deleteFavoriteData(final String itemId) {
        mManager.deleteValue(itemId);
    }

    /**
     * 删除指定类型的 所有数据
     *
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void deleteFavoriteDataByType(final int favoriteType) {
        mManager.deleteByFavoriteType(favoriteType);
    }


    /**
     * 查找 itemId 对应的本地数据是否为收藏点
     *
     * @param itemId 收藏点唯一码
     * @return true 已收藏，false 未收藏
     */
    public boolean isFavorite(final String itemId) {
        return mManager.isFavorite(itemId);
    }

    public static BehaviorPackage getInstance() {
        return Helper.EP;
    }

    @Override
    public void notifyAccountLogout(final int errCode, final int taskId, final AccountUserInfo result) {
        loginStatusChanged();
        for (BehaviorCallBack observer : mCallBacks) {
            //目前只需要登出状态
            observer.notifyLoginStatusChanged();
        }
    }

    @Override
    public void notifyQRCodeLoginConfirm(final int errCode, final int taskId, final AccountUserInfo result) {
        loginStatusChanged();
        for (BehaviorCallBack observer : mCallBacks) {
            observer.notifyLoginStatusChanged();
        }
    }


    /**
     * 登录状态发生改变时需要更新扎点
     */
    private void loginStatusChanged() {
        final String isFavoritePointStr = mSettingManager.getValueByKey(SettingController.KEY_SETTING_FAVORITE_POINT);
        mIsFavoriteDataUpdated =  !TextUtils.equals(isFavoritePointStr, "true");
        //清除旧扎点
        LayerPackage.getInstance().clearFavoriteMain(MapType.MAIN_SCREEN_MAIN_MAP);
        //显示家
        updateFavoriteMain(getHomeFavoriteInfo(), true);
        //显示公司
        updateFavoriteMain(getCompanyFavoriteInfo(), true);
        //显示收藏列表
        final ArrayList<PoiInfoEntity> poiInfoList = getFavoritePoiData();
        if (!ConvertUtils.isEmpty(poiInfoList)) {
            final LayerItemUserFavorite layerItemUserFavorite = new LayerItemUserFavorite();
            layerItemUserFavorite.setMSimpleFavoriteList(poiInfoList);
            LayerPackage.getInstance().addLayerItemOfFavorite(MapType.MAIN_SCREEN_MAIN_MAP, layerItemUserFavorite);
        }
    }

    private static final class Helper {
        private static final BehaviorPackage EP = new BehaviorPackage();
    }

    /**
     *  显示收藏夹图层，主图查看模式
     * @param poiInfoEntity
     * @param isAddFavorite
     */

    private void updateFavoriteMain(final PoiInfoEntity poiInfoEntity, final boolean isAddFavorite) {
        if (poiInfoEntity == null) {
            Logger.e(TAG, "the poi is null");
            return;
        }
        final String isFavoritePointStr = mSettingManager.getValueByKey(SettingController.KEY_SETTING_FAVORITE_POINT);
        final boolean isFavoritePoint = TextUtils.equals(isFavoritePointStr, "true");
        LayerPackage.getInstance().setFavoriteVisible(MapType.MAIN_SCREEN_MAIN_MAP, isFavoritePoint);
        mIsFavoriteDataUpdated = !isFavoritePoint;

        // 通知主图更新收藏点
        ThreadManager.getInstance().postDelay(() -> {
            if (isAddFavorite) {
                final LayerItemUserFavorite layerItemUserFavorite = new LayerItemUserFavorite();
                final ArrayList<PoiInfoEntity> poiInfoEntities = new ArrayList<>();
                poiInfoEntities.add(poiInfoEntity);
                layerItemUserFavorite.setMSimpleFavoriteList(poiInfoEntities);
                LayerPackage.getInstance().addLayerItemOfFavorite(MapType.MAIN_SCREEN_MAIN_MAP, layerItemUserFavorite);
            } else {
                LayerPackage.getInstance().removeFavoriteMain(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
            }
        }, 100);
    }

    /**
     * 获取开关关闭时收藏信息是否有更新
     * @return isFavoriteDataUpdated
     */
    public boolean getFavoriteDataUpdatedStatus() {
        return mIsFavoriteDataUpdated;
    }

    /**
     * 设置收藏信息是否更新状态
     * @param isSet
     */
    public void setFavoriteDataUpdatedStatus(final boolean isSet) {
        this.mIsFavoriteDataUpdated = isSet;
    }
}
