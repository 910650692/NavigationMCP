package com.fy.navi.scene.ui.navi;

import androidx.annotation.IntDef;

/**
 * @author QiuYaWei
 * @version $Revision.*$
 * Description: [充电桩Scene按钮类型]
 */
public final class SceneNaviChargeBtnType {
    private SceneNaviChargeBtnType() {
    }

    public static final int INVALID = -1; // 无效值
    public static final int OPEN_SUPPLY = 0; // 打开补能
    public static final int GO_CHARGING = 1; // 去充电--续航里程小于50km
    public static final int SEARCH_NEW_STATION = 3; // 查找新站--因为当前充电站拥挤
    public static final int I_KNOW = 4; // 我知道了，仅仅给用户提醒，不做其它处理
    public static final int UPDATE_SUPPLY = 5; // 刷新补能路线

    @IntDef({INVALID, OPEN_SUPPLY, GO_CHARGING, SEARCH_NEW_STATION, I_KNOW, UPDATE_SUPPLY})
    public @interface Type {

    }
}
