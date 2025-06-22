package com.sgm.navi.service.define.navi;

import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord3DDouble;

import java.util.ArrayList;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Description: 使用 Lombok 自动生成样板代码：
 * •@Data：自动生成 Getter、Setter、toString、equals 和 hashCode。
 * •@NoArgsConstructor：生成无参构造方法。
 * •@Accessors(chain = true)：支持链式调用，例如 entity.setName("POI").setCityCode("123");。
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class PathsTrafficEventInfoEntity {

    // 交通事件数组
    private ArrayList<PathTrafficEventInfoEntity> mPathTrafficEventInfoEntityList;
    // 路线的pathId
    private long mPathId;
    // 交通事件的action
    int mAction;


    @Data
    @NoArgsConstructor
    @Accessors(chain = true)
    static class PathTrafficEventInfoEntity {
        private int mType;
        private long mPathId;
        private int mLayer;
        private int mLayerTag;
        private int mId;
        private Coord2DDouble mCoord2D;
        private Coord3DDouble mCoord3D;
        private boolean mOfficial;
        private boolean mDetail;
        private String mLane;
        private int mLabel;
        private int mLaneId;
        private long mStartLinkId;
        private long mEndLinkId;
        private String mExt;
        private String mLabelDesc;
    }

}
