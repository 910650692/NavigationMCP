package com.fy.navi.service.logicpaket.user.behavior;

import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.user.behavior.BehaviorAdapter;
import com.fy.navi.service.adapter.user.behavior.BehaviorAdapterCallBack;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.favorite.Favorite;
import com.fy.navi.service.greendao.favorite.FavoriteManager;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

final public class BehaviorPackage implements BehaviorAdapterCallBack {
    private final BehaviorAdapter mBehaviorAdapter;
    private final List<BehaviorCallBack> mCallBacks = new ArrayList<>();
    private final FavoriteManager mManager;
    private final CommonManager mCommonManager;
    private final MapAdapter mapAdapter;
    private final LayerAdapter mLayerAdapter;

    private BehaviorPackage() {
        mBehaviorAdapter = BehaviorAdapter.getInstance();
        mManager = FavoriteManager.getInstance();
        mManager.init();
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
        mapAdapter = MapAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
    }

    /**
     * initBehaviorService
     */
    public void initBehaviorService() {
        mBehaviorAdapter.initBehaviorService();
        mBehaviorAdapter.registerCallBack("BehaviorPackage", this);
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

    /**
     * 判断用户是否已登录
     * @return 返回登录状态
     */
    private boolean isLogin() {
        boolean isLogin = false;
        final String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i("getUserInfo valueJson = " + valueJson);
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
            poiInfoEntity = mBehaviorAdapter.getHomeFavoriteInfo();
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
            poiInfoEntity = mBehaviorAdapter.getCompanyFavoriteInfo();
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
        if (isLogin() && type != 3) {
            return mBehaviorAdapter.addFavorite(poiInfo);
        }
        if (poiInfo != null && poiInfo.getFavoriteInfo() != null && TextUtils.isEmpty(poiInfo.getFavoriteInfo().getItemId())) {
            poiInfo.getFavoriteInfo().setItemId(poiInfo.getPid());
        }
        addFavoriteData(poiInfo, type);
        return "add favorite";
    }

    /**
     * 移除收藏点
     *
     * @param poiInfo
     * @return item id
     */
    public String removeFavorite(final PoiInfoEntity poiInfo) {
        if (isLogin()) {
            return mBehaviorAdapter.removeFavorite(poiInfo);
        }
        if (poiInfo != null && poiInfo.getFavoriteInfo() != null) {
            deleteFavoriteData(poiInfo.getFavoriteInfo().getItemId());
        }
        return "";
    }

    /**
     * 是否已收藏
     *
     * @param poiInfo
     * @return string
     */
    public String isFavorite(final PoiInfoEntity poiInfo) {
        if (isLogin()) {
            return mBehaviorAdapter.isFavorite(poiInfo);
        }
        if (poiInfo != null && !TextUtils.isEmpty(poiInfo.getPid())) {
            return isFavorite(poiInfo.getPid()) ? poiInfo.getPid() : "";
        }
        return "";
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
                        AppContext.getInstance().getMContext().getString(R.string.st_maximum_added_common_address));
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
        updateFavoriteMain();
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
    public PoiInfoEntity getFavoriteHomeData(final int favoriteType) {
        final List<Favorite> favoriteList = mManager.getValueByCommonName(favoriteType);
        if (null != favoriteList && !favoriteList.isEmpty()) {
            return getPoiInfoEntity(favoriteList.get(0));
        }
        return null;
    }

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
    public void deleteFavoriteData(final String itemId) {
        mManager.deleteValue(itemId);
        updateFavoriteMain();
    }

    /**
     * 删除指定类型的 所有数据
     *
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void deleteFavoriteDataByType(final int favoriteType) {
        mManager.deleteByFavoriteType(favoriteType);
        updateFavoriteMain();
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

    private static final class Helper {
        private static final BehaviorPackage EP = new BehaviorPackage();
    }

    /**
     *  显示收藏夹图层，主图查看模式
     */
    private void updateFavoriteMain() {
        // 通知主图更新收藏点
        final List<Favorite> tmpList = FavoriteManager.getInstance().getFavoriteNotTop();
        final ArrayList<GmBizUserFavoritePoint> list = new ArrayList<>();
        tmpList.forEach((rectFav -> {
            final GmBizUserFavoritePoint point = new GmBizUserFavoritePoint();
            point.favoriteType = rectFav.getMCommonName();
            point.lon = rectFav.getMPointX();
            point.lat = rectFav.getMPointY();
            list.add(point);
        }));
        mLayerAdapter.updateFavoriteMain(MapType.MAIN_SCREEN_MAIN_MAP, list);
    }
}
