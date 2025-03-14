package com.fy.navi.hmi.favorite;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorCallBack;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class CollectModel extends BaseModel<CollectViewModel> implements SettingCallback {
    private final BehaviorPackage behaviorPackage;
    private final SettingPackage settingPackage;
    private final MsgPushPackage msgPushPackage;
    public CollectModel() {
        behaviorPackage = BehaviorPackage.getInstance();

        settingPackage=SettingPackage.getInstance();
        settingPackage.registerCallBack(CollectModel.class.getSimpleName(),this);
        msgPushPackage = MsgPushPackage.getInstance();
    }

    /**
     * 获取精简收藏点列表（异步）
     */
    public ArrayList<PoiInfoEntity> getFavoriteListAsync() {
        return BehaviorPackage.getInstance().getFavoritePoiData(0);
    }

    @Override
    public void notify(int eventType, int exCode) {
        Logger.d("notifyFavorite: "+eventType+" exCode = "+exCode);
        mViewModel.setCollectData();
    }

    public List<PoiInfoEntity> getPushMsgList() {
        List<PoiInfoEntity> poiList = Optional.ofNullable(msgPushPackage.getAimPoiPushMessages())
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapMsgPushInfoToPoiInfoEntity)
                .collect(Collectors.toList());
        return poiList;
    }

    private PoiInfoEntity mapMsgPushInfoToPoiInfoEntity(MsgPushInfo msgPushInfo) {
        PoiInfoEntity entity = new PoiInfoEntity();
        entity.setName(msgPushInfo.getName());
        entity.setAddress(msgPushInfo.getAddress());
        GeoPoint point = new GeoPoint();
        point.lat = msgPushInfo.getLat();
        point.lon = msgPushInfo.getLon();
        entity.setPoint(point);
        entity.setPid(msgPushInfo.getPoiId());
        return entity;
    }
}
