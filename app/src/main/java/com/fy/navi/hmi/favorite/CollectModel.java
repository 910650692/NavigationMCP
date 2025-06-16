package com.fy.navi.hmi.favorite;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

public class CollectModel extends BaseModel<CollectViewModel> implements SettingCallback, SearchResultCallback {
    private final BehaviorPackage mBehaviorPackage;
    private final SettingPackage mSettingPackage;
    private final MsgPushPackage mMsgPushPackage;
    private final SearchPackage mSearchPackage;
    private final CalibrationPackage mCalibrationPackage;
    private final String mCallbackId;
    public CollectModel() {
        mBehaviorPackage = BehaviorPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mSettingPackage=SettingPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mSettingPackage.registerCallBack(CollectModel.class.getSimpleName(),this);
        mMsgPushPackage = MsgPushPackage.getInstance();
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage.registerCallBack(mCallbackId, this);
    }

    /**
     * 获取精简收藏点列表（异步）
     * @return list
     */
    public ArrayList<PoiInfoEntity> getFavoriteListAsync() {
        return BehaviorPackage.getInstance().getFavoritePoiData();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mSearchPackage != null) {
            mSearchPackage.unRegisterCallBack(mCallbackId);
        }
    }

    @Override
    public void notify(final int eventType, final int exCode) {
        Logger.d("notifyFavorite: "+eventType+" exCode = "+exCode);
        mViewModel.setCollectData();
    }

    /**
     * getPushMsgList
     * @return list
     */
    public List<PoiInfoEntity> getPushMsgList() {
        final List<PoiInfoEntity> poiList = Optional.ofNullable(mMsgPushPackage.getAimPoiPushMessages())
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapMsgPushInfoToPoiInfoEntity)
                .collect(Collectors.toList());
        return poiList;
    }

    /**
     * mapMsgPushInfoToPoiInfoEntity
     * @param msgPushInfo
     * @return entity
     */
    private PoiInfoEntity mapMsgPushInfoToPoiInfoEntity(final MsgPushInfo msgPushInfo) {
        final PoiInfoEntity entity = new PoiInfoEntity();
        entity.setName(msgPushInfo.getName());
        entity.setAddress(msgPushInfo.getAddress());
        final GeoPoint point = new GeoPoint();
        point.setLat(ConvertUtils.transCityLatAndLon(msgPushInfo.getLat()));
        point.setLon(ConvertUtils.transCityLatAndLon(msgPushInfo.getLon()));
        entity.setPoint(point);
        entity.setPid(msgPushInfo.getPoiId());
        entity.setDistance(mSearchPackage.calcStraightDistance(point));
        return entity;
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mCalibrationPackage.powerType();
    }


    @Override
    public void onNetSearchResult(int taskId,String searchKey,BaseRep result) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mCallbackId: " + mCallbackId + "currentCallbackId: " +mSearchPackage.getCurrentCallbackId());
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if(AutoMapConstant.NetSearchKey.QUERY_COLLECT_LIST.equals(searchKey)){
                mViewModel.notifyNetSearchResult(taskId,result);
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }

    @Override
    public void onNetSearchResultError(int taskId, String searchKey, String message) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mCallbackId: " + mCallbackId + "currentCallbackId: " +mSearchPackage.getCurrentCallbackId());
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            if(AutoMapConstant.NetSearchKey.QUERY_COLLECT_LIST.equals(searchKey)){
                mViewModel.notifyNetSearchResultError(taskId,message);
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }
}
