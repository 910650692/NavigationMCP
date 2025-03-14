package com.fy.navi.service.logicpaket.user.behavior;

import android.text.TextUtils;

import com.android.utils.ToastUtils;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.user.behavior.BehaviorAdapter;
import com.fy.navi.service.adapter.user.behavior.BehaviorAdapterCallBack;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.favorite.Favorite;
import com.fy.navi.service.greendao.favorite.FavoriteManager;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

/**
 * @Description 数据收藏package管理
 * @Author fh
 * @date 2024/12/26
 */
public class BehaviorPackage implements BehaviorAdapterCallBack {
    private final BehaviorAdapter mBehaviorAdapter;
    private final List<BehaviorCallBack> callBacks = new ArrayList<>();
    private final FavoriteManager mManager;
    private final MapAdapter mapAdapter;
    private final LayerAdapter layerAdapter;

    private BehaviorPackage() {
        mBehaviorAdapter = BehaviorAdapter.getInstance();
        mManager = FavoriteManager.getInstance();
        mapAdapter = MapAdapter.getInstance();
        layerAdapter = LayerAdapter.getInstance();
        mManager.init();
    }

    public void initBehaviorService() {
        mBehaviorAdapter.initBehaviorService();
        mBehaviorAdapter.registerCallBack("BehaviorPackage", this);
    }

    public synchronized void registerCallBack(BehaviorCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.add(callback);
        }
    }

    public void unInitBehaviorService() {
        mBehaviorAdapter.unInitBehaviorService();
    }

    public int[] getSimpleFavoriteIds() {
        return mBehaviorAdapter.getSimpleFavoriteIds();
    }

    /**
     * 获取家的数据
     */
    public PoiInfoEntity getHomeFavoriteInfo() {
        return mBehaviorAdapter.getHomeFavoriteInfo();
    }

    /**
     * 获取公司的数据
     */
    public PoiInfoEntity getCompanyFavoriteInfo() {
        return mBehaviorAdapter.getCompanyFavoriteInfo();
    }

    /**
     * 获取普通收藏点的数据
     */
    public ArrayList<PoiInfoEntity> getSimpleFavoriteList() {
        return mBehaviorAdapter.getSimpleFavoriteList();
    }

    public PoiInfoEntity getFavorite(PoiInfoEntity baseInfo) {
        return mBehaviorAdapter.getFavorite(baseInfo);
    }

    /**
     * 添加收藏点
     * 注意区分收藏点类型  common_name：1，家  2，公司  0，普通收藏点
     *
     * @param poiInfo
     * @return
     */
    public String addFavorite(PoiInfoEntity poiInfo) {
        return mBehaviorAdapter.addFavorite(poiInfo);
    }

    /**
     * 移除收藏点
     *
     * @param poiInfo
     * @return
     */
    public String removeFavorite(PoiInfoEntity poiInfo) {
        return mBehaviorAdapter.removeFavorite(poiInfo);
    }

    /**
     * 是否已收藏
     *
     * @param poiInfo
     * @return
     */
    public String isFavorite(PoiInfoEntity poiInfo) {
        return mBehaviorAdapter.isFavorite(poiInfo);
    }

    public int getFavoriteListAsync(int type, boolean sorted) {
        return mBehaviorAdapter.getFavoriteListAsync(type, sorted);
    }

    public String topFavorite(PoiInfoEntity baseInfo, boolean bSetTop) {
        return mBehaviorAdapter.topFavorite(baseInfo, bSetTop);
    }

    public String modifyFavorite(PoiInfoEntity detailInfo, String customName) {
        return mBehaviorAdapter.modifyFavorite(detailInfo, customName);
    }

    public void startSync() {
        mBehaviorAdapter.startSync();
    }

    @Override
    public void notifyFavorite(int eventType, int exCode) {
        for (BehaviorCallBack observer : callBacks) {
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
    public void notifyFavoriteAsync(int type, ArrayList<PoiInfoEntity> data, boolean sorted) {
        for (BehaviorCallBack observer : callBacks) {
            observer.notifyFavoriteAsync(type, data, sorted);
        }
    }

    /**
     * 添加收藏夹信息到本地数据库
     *
     * @param entity
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return
     */
    public void addFavoriteData(PoiInfoEntity entity, int favoriteType) {
        if (favoriteType == 3) {
            ArrayList<PoiInfoEntity> commonList = getFavoritePoiData(3);
            if (commonList.size() >= 3) {
                ToastUtils.Companion.getInstance().showCustomToastView(AppContext.mContext.getString(R.string.st_maximum_added_common_address));
                return;
            }
        }
        
        Favorite favorite = new Favorite();
        favorite.itemId = entity.getFavoriteInfo().getItemId();
        favorite.commonName = favoriteType;
        favorite.customName = entity.getFavoriteInfo().getCustom_name();
        favorite.pid = entity.getPid();
        favorite.name = entity.getName();
        favorite.phone = entity.getPhone();
        favorite.distance = formatDistance(entity.getDistance());
//        favorite.cityName = entity.getCityInfo().getCityName();
//        favorite.cityCode = String.valueOf(entity.getCityInfo().getCityCode());
        favorite.address = entity.getAddress();
        favorite.point_x = entity.getPoint().lon;
        favorite.point_y = entity.getPoint().lat;
//        favorite.point_x_arrive = entity.arrivePoint;
//        favorite.point_y_arrive = entity.arrivePoint;
        favorite.updateTime = new Date();
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
    private String formatDistance(String distance) {
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
     * @return
     */
    public PoiInfoEntity getFavoriteHomeData(int favoriteType) {
        List<Favorite> favoriteList = mManager.getValueByCommonName(favoriteType);
        if (null != favoriteList && !favoriteList.isEmpty()) {
            return getPoiInfoEntity(favoriteList.get(0));
        }
        return null;
    }

    /**
     * 在本地获取常去地址/普通收藏点的信息
     *
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return
     */
    public ArrayList<PoiInfoEntity> getFavoritePoiData(int favoriteType) {
        List<Favorite> favoriteList = mManager.getValueByCommonName(favoriteType);
        //HMI进行业务处理
        ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        if (favoriteList != null && !favoriteList.isEmpty()) {
            for (Favorite item : favoriteList) {
                PoiInfoEntity simpleFavoriteInfo = getPoiInfoEntity(item);
                dataList.add(simpleFavoriteInfo);
            }
        }

        dataList.sort((poiInfoEntity1, poiInfoEntity2) -> {
            boolean aIsTop = poiInfoEntity1.getFavoriteInfo().getTop_time() != 0;
            boolean bIsTop = poiInfoEntity2.getFavoriteInfo().getTop_time() != 0;

            // 第一优先级：置顶状态
            if (aIsTop != bIsTop) {
                return aIsTop ? -1 : 1; // 置顶项始终在前
            }

            // 第二优先级（仅当两者都置顶时）：top_time倒序
            if (aIsTop) {
                long topDiff = poiInfoEntity2.getFavoriteInfo().getTop_time() - poiInfoEntity1.getFavoriteInfo().getTop_time();
                if (topDiff != 0)
                    return Long.compare(poiInfoEntity2.getFavoriteInfo().getTop_time(), poiInfoEntity1.getFavoriteInfo().getTop_time());
            }

            // 第三优先级：updateTime倒序
            return Long.compare(poiInfoEntity2.getFavoriteInfo().getUpdateTime(), poiInfoEntity1.getFavoriteInfo().getUpdateTime());
        });
        return dataList;
    }

    /**
     * 获取未置顶列表
     *
     * @return 未置顶列表
     */
    public ArrayList<PoiInfoEntity> getFavoriteNotTop() {
        List<Favorite> favoriteList = mManager.getFavoriteNotTop();
        ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        if (favoriteList != null && !favoriteList.isEmpty()) {
            for (Favorite item : favoriteList) {
                PoiInfoEntity simpleFavoriteInfo = getPoiInfoEntity(item);
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
        List<Favorite> favoriteList = mManager.getValueByTopTime();
        ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        if (favoriteList != null && !favoriteList.isEmpty()) {
            for (Favorite item : favoriteList) {
                PoiInfoEntity simpleFavoriteInfo = getPoiInfoEntity(item);
                dataList.add(simpleFavoriteInfo);
            }
        }
        dataList.sort((poiInfoEntity1, poiInfoEntity2) -> {

            long topDiff = poiInfoEntity2.getFavoriteInfo().getTop_time() - poiInfoEntity1.getFavoriteInfo().getTop_time();
            if (topDiff != 0)
                return Long.compare(poiInfoEntity2.getFavoriteInfo().getTop_time(), poiInfoEntity1.getFavoriteInfo().getTop_time());

            return Long.compare(poiInfoEntity2.getFavoriteInfo().getUpdateTime(), poiInfoEntity1.getFavoriteInfo().getUpdateTime());
        });
        return dataList;
    }

    public PoiInfoEntity getPoiInfoEntity(Favorite item) {
        FavoriteInfo info = new FavoriteInfo()
                .setItemId(item.itemId)
                .setCommonName(item.commonName)
                .setTag(item.tag)
                .setType(item.type)
                .setNewType(item.newType)
                .setCustom_name(item.customName)
                .setClassification(item.classification)
                .setUpdateTime(item.updateTime.getTime())
                .setTop_time(item.topTime);

        PoiInfoEntity simpleFavoriteInfo = new PoiInfoEntity()
                .setPid(String.valueOf(item.pid))
                .setAddress(item.address)
                .setName(item.name)
                .setPhone(item.phone)
                .setDistance(item.distance)
                .setPoint(new GeoPoint(item.point_x, item.point_y))
                .setFavoriteInfo(info);
        return simpleFavoriteInfo;
    }

    /**
     * 重命名时需更新本地数据
     *
     * @param itemId     收藏点唯一码
     * @param customName 自定义名称 重命名时编辑的字段
     */
    public void modifyFavoriteData(String itemId, String customName) {
        mManager.updateCustomName(itemId, customName);
    }

    /**
     * 重命名时需更新本地数据
     *
     * @param itemId  收藏点唯一码
     * @param topTime 置顶时间
     */
    public void updateFavoriteTopTime(String itemId, long topTime) {
        mManager.updateTopTime(itemId, topTime);
    }

    /**
     * 删除 itemId 对应的本地数据
     *
     * @param itemId 收藏点唯一码
     */
    public void deleteFavoriteData(String itemId) {
        mManager.deleteValue(itemId);
        updateFavoriteMain();
    }

    /**
     * 删除指定类型的 所有数据
     *
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void deleteFavoriteDataByType(int favoriteType) {
        mManager.deleteByFavoriteType(favoriteType);
        updateFavoriteMain();
    }


    /**
     * 查找 itemId 对应的本地数据是否为收藏点
     *
     * @param itemId 收藏点唯一码
     * @return true 已收藏，false 未收藏
     */
    public boolean isFavorite(String itemId) {
        return mManager.isFavorite(itemId);
    }

    public static BehaviorPackage getInstance() {
        return Helper.ep;
    }

    private static final class Helper {
        private static final BehaviorPackage ep = new BehaviorPackage();
    }

    //显示收藏夹图层，主图查看模式
    private void updateFavoriteMain() {
        // 通知主图更新收藏点
        List<Favorite> tmpList = FavoriteManager.getInstance().getFavoriteNotTop();
        ArrayList<GmBizUserFavoritePoint> list = new ArrayList<>();
        tmpList.forEach((rectFav -> {
            GmBizUserFavoritePoint point = new GmBizUserFavoritePoint();
            point.favoriteType = rectFav.commonName;
            point.lon = rectFav.point_x;
            point.lat = rectFav.point_y;
            list.add(point);
        }));
        layerAdapter.updateFavoriteMain(MapTypeId.MAIN_SCREEN_MAIN_MAP, list);
    }
}
