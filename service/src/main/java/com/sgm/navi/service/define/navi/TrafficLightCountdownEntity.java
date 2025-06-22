package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TrafficLightCountdownEntity {

    //路线ID
    private long mPathID;
    //路况状态 @range [ 1: 极度拥堵, 2: 拥堵, 3: 拥堵+缓行, 4: 缓行, 5: 畅通 ]
    private int mStatus;
    //所在linkID值
    private long mLinkID;
    //segment导航段索引
    private long mSegmentIndex;
    //link段索引
    private long mLinkIndex;
    //所在位置
    private GeoPoint mPosition;
    //实时灯态信息
    private LightInfoEntity mLightInfo;

    @NonNull
    @Override
    public String toString() {
        return "TrafficLightCountdownEntity{" +
                "pathID=" + mPathID +
                ", status=" + mStatus +
                ", linkID=" + mLinkID +
                ", segmentIndex=" + mSegmentIndex +
                ", linkIndex=" + mLinkIndex +
                ", position=" + mPosition +
                ", lightInfo=" + mLightInfo +
                '}';
    }
}
