package com.sgm.navi.service.adapter.user.behavior.bls;

import android.annotation.SuppressLint;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.behavior.BehaviorService;
import com.autonavi.gbl.user.behavior.model.FavoriteBaseItem;
import com.autonavi.gbl.user.behavior.model.FavoriteItem;
import com.autonavi.gbl.user.behavior.model.FavoriteType;
import com.autonavi.gbl.user.behavior.model.SimpleFavoriteItem;
import com.autonavi.gbl.user.syncsdk.SyncSdkService;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.user.behavior.BehaviorAdapterCallBack;
import com.sgm.navi.service.adapter.user.behavior.IBehaviorApi;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

public class BehaviorAdapterImpl implements IBehaviorApi {
    private static final String TAG = MapDefaultFinalTag.FAVORITE_SERVICE_TAG;
    public static final float BASE_POINT = 3600000.0F;
    private BehaviorAdapterImplHelper mAdapterImplHelper;
    private BehaviorService mBehaviorService;
    private SyncSdkService mSyncSdkService;

    public BehaviorAdapterImpl() {
    }

    @Override
    public void initBehaviorService() {
        if(null == mBehaviorService) mBehaviorService = (BehaviorService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.BehaviorSingleServiceID);
        mSyncSdkService = (SyncSdkService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.SyncSdkSingleServiceID);
        mAdapterImplHelper = new BehaviorAdapterImplHelper(mBehaviorService, mSyncSdkService);
        mAdapterImplHelper.initBehaviorService();
    }

    @Override
    public void setLoginInfo() {
        mAdapterImplHelper.setLoginInfo();
    }

    @Override
    public void registerCallBack(final String key, final BehaviorAdapterCallBack callBack) {
        mAdapterImplHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(final String key) {
        mAdapterImplHelper.unRegisterCallBack(key);
    }

    @Override
    public void unInitBehaviorService() {
        if (mBehaviorService != null) {
            mAdapterImplHelper.removeCallback();
            if (ServiceInitStatus.ServiceInitDone != mBehaviorService.isInit()) {
                mBehaviorService.unInit();
            }
        }
    }

    /**
     * 获取收藏点id列表（收藏点个数）
     */
    @Override
    public int[] getSimpleFavoriteIds() {
        if (mBehaviorService == null) {
            return null;
        }
        return mBehaviorService.getSimpleFavoriteIds();
    }

    /**
     * 获取家的信息（同步方式）
     * @return
     */
    @Override
    public PoiInfoEntity getHomeFavoriteInfo() {
        if (mBehaviorService == null) {
            return null;
        }
        final ArrayList<SimpleFavoriteItem> simpleFavoriteList =
                mBehaviorService.getSimpleFavoriteList(FavoriteType.FavoriteTypeHome, true);
        if (simpleFavoriteList != null) {
            Logger.i(TAG, "getHomeFavoriteInfo: " + simpleFavoriteList.size());
            for (int i = 0; i < simpleFavoriteList.size(); i++) {
                Logger.i(TAG, "getHomeFavoriteInfo: " + i + "="
                        + simpleFavoriteList.get(i).id + "; "
                        + simpleFavoriteList.get(i).item_id + "; "
                        + simpleFavoriteList.get(i).address + "; "
                        + simpleFavoriteList.get(i).city_name + "; "
                        + simpleFavoriteList.get(i).type + "; "
                        + simpleFavoriteList.get(i).point_x + "; "
                        + simpleFavoriteList.get(i).point_y + "; "
                        + simpleFavoriteList.get(i).name
                );
            }
        } else {
            Logger.i(TAG, "getHomeFavoriteInfo: simpleFavoriteList == null");
        }
        return getPoiInfoEntity(simpleFavoriteList);
    }

    /**
     * 获取公司的信息（同步方式）
     * @return
     */
    @Override
    public PoiInfoEntity getCompanyFavoriteInfo() {
        if (mBehaviorService == null) {
            return null;
        }
        final ArrayList<SimpleFavoriteItem> simpleFavoriteList =
                mBehaviorService.getSimpleFavoriteList(FavoriteType.FavoriteTypeCompany, true);
        if (simpleFavoriteList != null) {
            Logger.i(TAG, "getCompanyFavoriteInfo: " + simpleFavoriteList.size());
            for (int i = 0; i < simpleFavoriteList.size(); i++) {
                Logger.i(TAG, "getCompanyFavoriteInfo: " + i + "="
                        + simpleFavoriteList.get(i).address + "; "
                        + simpleFavoriteList.get(i).city_name + "; "
                        + simpleFavoriteList.get(i).type + "; "
                        + simpleFavoriteList.get(i).point_x + "; "
                        + simpleFavoriteList.get(i).point_y + "; "
                        + simpleFavoriteList.get(i).name
                );
            }
        } else {
            Logger.i(TAG, "getCompanyFavoriteInfo: simpleFavoriteList == null");
        }
        return getPoiInfoEntity(simpleFavoriteList);
    }

    /**
     * getPoiInfoEntity
     * @param simpleFavoriteList
     * @return PoiInfoEntity
     */
    public PoiInfoEntity getPoiInfoEntity(final ArrayList<SimpleFavoriteItem> simpleFavoriteList) {
        //HMI进行业务处理
        PoiInfoEntity poiInfoEntity = null;
        if (simpleFavoriteList != null && !simpleFavoriteList.isEmpty()) {
            final SimpleFavoriteItem item = simpleFavoriteList.get(0);
            final FavoriteInfo info = new FavoriteInfo()
                    .setItemId(item.item_id)
                    .setCommonName(item.common_name)
                    .setTag(item.tag)
                    .setType(item.type)
                    .setNewType(item.newType)
                    .setCustom_name(item.custom_name)
                    .setClassification(item.classification)
                    .setTop_time(item.top_time);

            final PoiInfoEntity simpleFavoriteInfo = new PoiInfoEntity()
//                    .setPid(String.valueOf(item.id))
                    .setAdCode(ConvertUtils.str2Int(item.city_code))
                    .setAddress(item.address)
                    .setPhone(item.phone_numbers)
                    .setPoint(new GeoPoint(item.point_x / BASE_POINT, item.point_y / BASE_POINT))
                    .setName(item.name)
                    .setFavoriteInfo(info);

            poiInfoEntity = simpleFavoriteInfo;
        }
        return poiInfoEntity;
    }

    /**
     * 获取精简收藏点列表（同步方式）
     */
    @Override
    public ArrayList<PoiInfoEntity> getSimpleFavoriteList() {
        if (mBehaviorService == null) {
            return null;
        }
        final ArrayList<SimpleFavoriteItem> simpleFavoriteList =
                mBehaviorService.getSimpleFavoriteList(FavoriteType.FavoriteTypePoi, true);
        //HMI进行业务处理
        final ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        if (simpleFavoriteList != null && simpleFavoriteList.size() > 0) {
            for (SimpleFavoriteItem item : simpleFavoriteList) {
                final FavoriteInfo info = new FavoriteInfo()
                        .setItemId(item.item_id)
                        .setCommonName(item.common_name)
                        .setTag(item.tag)
                        .setType(item.type)
                        .setNewType(item.newType)
                        .setCustom_name(item.custom_name)
                        .setClassification(item.classification)
                        .setTop_time(item.top_time);

                PoiInfoEntity simpleFavoriteInfo = new PoiInfoEntity()
//                        .setPid(String.valueOf(item.id))
                        .setAdCode(ConvertUtils.str2Int(item.city_code))
                        .setAddress(item.address)
                        .setPhone(item.phone_numbers)
                        .setPoint(new GeoPoint(item.point_x / BASE_POINT, item.point_y / BASE_POINT))
                        .setName(item.name)
                        .setFavoriteInfo(info);
                simpleFavoriteInfo = getFavorite(simpleFavoriteInfo);
                dataList.add(simpleFavoriteInfo);
            }
        } else {
            Logger.d(TAG, "getSimpleFavoriteList: list is null");
        }
        return dataList;
    }

    /**
     * 获取精简收藏点列表（异步方式）
     */
    @Override
    public int getFavoriteListAsync(final int type, final boolean sorted) {
        if (mBehaviorService == null) {
            return -1;
        }
        return mBehaviorService.getFavoriteListAsync(FavoriteType.FavoriteTypePoi, true);
    }

    /**
     * 获取收藏点详细信息
     */
    @Override
    public PoiInfoEntity getFavorite(final PoiInfoEntity baseInfo) {
        if (mBehaviorService == null || baseInfo == null || baseInfo.getFavoriteInfo() == null) {
            Logger.d(TAG, "get favorite is null");
            return null;
        }
        final FavoriteBaseItem baseItem = new FavoriteBaseItem();
        baseItem.item_id = baseInfo.getFavoriteInfo().getItemId();
        baseItem.poiid = baseInfo.getPid();
        baseItem.point_x = (int) (baseInfo.getPoint().getLon() * BASE_POINT);
        baseItem.point_y = (int) (baseInfo.getPoint().getLat() * BASE_POINT);
        baseItem.name = baseInfo.getName();

        final FavoriteItem favoriteItem = mBehaviorService.getFavorite(baseItem);
        //HMI进行业务处理
        if (favoriteItem != null) {
            final String poiId = favoriteItem.poiid;
            final FavoriteInfo favoriteInfo = new FavoriteInfo()
                    .setItemId(favoriteItem.item_id)
                    .setCommonName(favoriteItem.common_name)
                    .setTag(favoriteItem.tag)
                    .setType(favoriteItem.type)
                    .setNewType(favoriteItem.newType)
                    .setCustom_name(favoriteItem.custom_name)
                    .setClassification(favoriteItem.classification)
                    .setTop_time(favoriteItem.top_time);
            final PoiInfoEntity info = new PoiInfoEntity()
                    .setPid(poiId)
                    .setAdCode(ConvertUtils.str2Int(favoriteItem.city_code))
                    .setAddress(favoriteItem.address)
                    .setPhone(favoriteItem.phone_numbers)
                    .setName(favoriteItem.name)
                    .setFavoriteInfo(favoriteInfo);

            if (!TextUtils.isEmpty(poiId) && poiId.contains("_")) {
                String[] points = poiId.split("_");
                if (points.length == 2) {
                    final double x = parseDoubleSafely(points[0], 0.0);
                    final double y = parseDoubleSafely(points[1], 0.0);
                    info.setPoint(new GeoPoint(x, y));
                }
            }else {
                info.setPoint(new GeoPoint(baseInfo.getPoint().getLon(), baseInfo.getPoint().getLat()));
            }
            return info;
        }
        return baseInfo;
    }

    public static double parseDoubleSafely(String str, double defaultValue) {
        try {
            return Double.parseDouble(str);
        } catch (NumberFormatException e) {
            Logger.e("ParseError", "Invalid number: " + str, e);
            return defaultValue;
        }
    }

    /**
     * 添加收藏点
     */
    @SuppressLint("WrongConstant")
    @Override
    public String addFavorite(final PoiInfoEntity poiInfo) {
        if (mBehaviorService == null || poiInfo == null || poiInfo.getFavoriteInfo() == null) {
            Logger.d(TAG, "add favorite is null");
            return "";
        }
        final FavoriteItem item = new FavoriteItem();
        item.item_id = poiInfo.getFavoriteInfo().getItemId();
        item.poiid = poiInfo.getPid();
        item.address = poiInfo.getAddress();
        item.common_name = poiInfo.getFavoriteInfo().getCommonName();
        item.name = poiInfo.getName();
        item.point_x = (int) (poiInfo.getPoint().getLon() * BASE_POINT);
        item.point_y = (int) (poiInfo.getPoint().getLat() * BASE_POINT);
        // 添加成功返回 FavoriteItem 对应的存档ID
        final String result = mBehaviorService.addFavorite(item, SyncMode.SyncModeNow);
        Logger.d(TAG, "addFavorite result = ", result , " " , item.point_x);
        return result;
    }

    /**
     * 删除收藏点
     */
    @Override
    public String removeFavorite(final PoiInfoEntity poiInfo) {
        if (mBehaviorService == null || poiInfo == null || poiInfo.getFavoriteInfo() == null) {
            Logger.d(TAG, "remove favorite is null");
            return "";
        }
        final FavoriteBaseItem delItem = new FavoriteBaseItem();
        delItem.item_id = poiInfo.getFavoriteInfo().getItemId();
        delItem.poiid = poiInfo.getPid();
        delItem.point_x = (int) (poiInfo.getPoint().getLon() * BASE_POINT);
        delItem.point_y = (int) (poiInfo.getPoint().getLat() * BASE_POINT);
        delItem.name = poiInfo.getName();
        Logger.d(TAG, "delFavorite", GsonUtils.toJson(delItem));
        // 删除成功返回 FavoriteBaseItem对应的存档ID
        final String result = mBehaviorService.delFavorite(delItem, SyncMode.SyncModeNow);
        Logger.d(TAG, "removeFavorite ret = ", result);
        return result;
    }

    /**
     * 是否是收藏点
     * 当绘制收藏夹图层的时候将item_id调用setid接口设置给图层，点击图层的时候通过getid接口获取itemid赋值给
     * FavoriteBaseItem.item_id来判断是否收藏，避免通过转换精度丢失导致判断出错问题
     */
    @Override
    public String isFavorite(final PoiInfoEntity poiInfo) {
        if (mBehaviorService == null || poiInfo == null) {
            Logger.d(TAG, "is favorite is null");
            return "";
        }
        final FavoriteBaseItem favoriteInfo = new FavoriteBaseItem();
        if (!TextUtils.isEmpty(poiInfo.getPid()) && !poiInfo.getPid().contains(".")) {
            favoriteInfo.poiid = poiInfo.getPid();
        }
        favoriteInfo.point_x = (int) (poiInfo.getPoint().getLon() * BASE_POINT);
        favoriteInfo.point_y = (int) (poiInfo.getPoint().getLat() * BASE_POINT);
        favoriteInfo.name = poiInfo.getName();
        // 返回 收藏点存档ID 表示已收藏
        final String result = mBehaviorService.isFavorited(favoriteInfo);
        Logger.d(TAG, "isFavorited result = ", result);
        return result;
    }

    /**
     * 收藏点置顶/取消置顶
     *
     * @param info    置顶item信息
     * @param isSetTop 是否置顶 true 置顶；false 取消置顶
     */
    @Override
    public String topFavorite(final PoiInfoEntity info, final boolean isSetTop) {
        if (mBehaviorService == null || info == null || info.getFavoriteInfo() == null) {
            Logger.d(TAG, "top favorite is null");
            return "";
        }
        final FavoriteBaseItem baseItem = new FavoriteBaseItem();
        baseItem.item_id = info.getFavoriteInfo().getItemId();
        baseItem.poiid = info.getPid();
        baseItem.point_x = (int) (info.getPoint().getLon() * BASE_POINT);
        baseItem.point_y = (int) (info.getPoint().getLat() * BASE_POINT);
        baseItem.name = info.getName();
        final String ret = mBehaviorService.topFavorite(baseItem, isSetTop, SyncMode.SyncModeNow);
        Logger.d(TAG, "topFavorite ret = ", ret);
        return ret;
    }

    /**
     * 收藏点重命名
     */
    @Override
    public String modifyFavorite(final PoiInfoEntity detailInfo, final String customName) {
        if (mBehaviorService == null || detailInfo == null || detailInfo.getFavoriteInfo() == null)  {
            Logger.d(TAG, "modify favorite is null");
            return "";
        }
        // 1 获取收藏点详细数据
        final FavoriteBaseItem baseItem = new FavoriteBaseItem();
        baseItem.item_id = detailInfo.getFavoriteInfo().getItemId();
        baseItem.poiid = detailInfo.getPid();
        baseItem.point_x = (int) (detailInfo.getPoint().getLon() * BASE_POINT);
        baseItem.point_y = (int) (detailInfo.getPoint().getLat() * BASE_POINT);
        baseItem.name = detailInfo.getName();
        final FavoriteItem detailItem = mBehaviorService.getFavorite(baseItem);
        // 2 重命名
        detailItem.custom_name = customName; // "重命名";
        final int mode = SyncMode.SyncModeNow; // SyncModeLater 稍后同步
        Logger.d(TAG, "updateFavorite", GsonUtils.toJson(detailInfo));
        final String ret = mBehaviorService.updateFavorite(detailItem, mode);
        Logger.d(TAG, "updateFavorite ret = ", ret);
        return ret;
    }

    @Override
    public String addFrequentAddress(final PoiInfoEntity poiInfo) {
        return "";
    }

    /**
     * 触发手动同步，首先获取一下同步库当前是否正在同步
     */
    @Override
    public void startSync() {
        if (mSyncSdkService.isSyncing() != 0) {
            mSyncSdkService.startSync();
        }

        // 同步常去地点（家、公司）数据
        mAdapterImplHelper.syncFrequentData();
    }

}
