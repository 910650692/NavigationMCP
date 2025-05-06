package com.fy.navi.hmi.favorite;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class CollectModel extends BaseModel<CollectViewModel> implements SettingCallback {
    private final BehaviorPackage mBehaviorPackage;
    private final SettingPackage mSettingPackage;
    private final MsgPushPackage mMsgPushPackage;
    private final CalibrationPackage mCalibrationPackage;
    public CollectModel() {
        mBehaviorPackage = BehaviorPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mSettingPackage=SettingPackage.getInstance();
        mSettingPackage.registerCallBack(CollectModel.class.getSimpleName(),this);
        mMsgPushPackage = MsgPushPackage.getInstance();
    }

    /**
     * 获取精简收藏点列表（异步）
     * @return list
     */
    public ArrayList<PoiInfoEntity> getFavoriteListAsync() {
        return BehaviorPackage.getInstance().getFavoritePoiData();
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
}
