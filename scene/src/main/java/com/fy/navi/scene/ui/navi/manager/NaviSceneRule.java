package com.fy.navi.scene.ui.navi.manager;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;

/**
 * 定义碰撞逻辑的matrix表;
 * 以2维数组表示，行代表的是新卡片，列代表的是当前显示卡片(旧卡)；
 * 显示优先级根据UE及产品文档定义;
 * <p>
 * 卡片显隐规则如下:
 * 双卡同时显示：   SceneShowAndShow    0
 * 显示旧卡关闭新卡：SceneShowAndClose   1
 * 显示旧卡隐藏新卡：SceneShowAndHide    2
 * 隐藏旧卡显示新卡：SceneHideAndShow    3
 * 关闭旧卡显示新卡：SceneCloseAndShow   4
 * *************：SceneIgnore         5 显示新卡时，旧卡不做处理，根据自身状态显示/隐藏
 * *************：SceneInvalid       -1 无效场景
 * <p>
 * 新卡\旧卡 2D路口大图|3D路口大图|eta信息|车道线信息|tbt信息|途经点收起|途经点展开|途经点到达| 详情卡片 |服务区卡片|继续导航按钮|平行路切换|停车场推荐|控制tools|最后一公里|路线偏好|区间测速|路况条信息|当前道路名称|途经点到达弹窗
 * 2D路口	   -1     	3        0       2       5      -1        3        2        -1         2        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 3D路口	    2	   -1        0       5       5      -1       -1        5        -1        -1        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * eat信息	    0       0        0       0       5       0        3        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 车道线信息     3       0        0      -1       5       0        3        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * tbt信息       0       0        0       5      -1       0        3        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 途经点收起	    4	    5        0      -1       5      -1       -1        5        -1        -1        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 途经点展开	    4	    5        0       3       5      -1       -1        5        -1         2        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 途经点到达	    3	    5        0       5       5       5        3       -1        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 详情卡片	    4	    5        0       5       5       5       -1        5        -1         2        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 服务区卡片	    3	    5        0       5       5       5        3        5        -1        -1        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 继续导航按钮   5	    5        0       5       5       5        5        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 平行路切换	    5       5        0       5       5       5        5        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 停车场推荐    	3	    5        0       5       5       5        3        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 控制tools     4       4        0       5       5       5        4        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 最后一公里     0       5        0       5       5       5        5        5        -1         5        -1        5        -1      -1        -1      -1      -1      -1        -1         5
 * 路线偏好	    4       4        0       5       5       5        4        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 区间测速	    0       5        0       5       5       5        5        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 路况条信息	    3       0        0       5       5       0        3        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 当前道路名称   5       3        0       5       5       5        5        5        -1         5        -1        5        -1      -1         5      -1      -1      -1        -1         5
 * 途经点到达弹窗 0       5        0       5       5       5        5        5        -1         5        -1        5        -1      -1        -1      -1      -1      -1        -1         5
 */
public class NaviSceneRule {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    public final static int SceneInvalid = -1;
    public final static int SceneShowAndShow = 0;
    public final static int SceneShowAndClose = 1;
    public final static int SceneShowAndHide = 2;
    public final static int SceneHideAndShow = 3;
    public final static int SceneCloseAndShow = 4;
    public final static int SceneIgnore = 5;

    private static final int[][] COLLISION_MATRIX = {
            {-1, 3, 0, 2, 5, -1, 3, 2, -1, 2, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {2, -1, 0, 5, 5, -1, -1, 5, -1, -1, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {0, 0, 0, 0, 5, 0, 3, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {3, 0, 0, -1, 5, 0, 3, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {0, 0, 0, 5, -1, 0, 3, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {4, 5, 0, -1, 5, -1, -1, 5, -1, -1, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {4, 5, 0, 3, 5, -1, -1, 5, -1, 2, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {3, 5, 0, 5, 5, 5, 3, -1, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {4, 5, 0, 5, 5, 5, -1, 5, -1, 2, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {3, 5, 0, 5, 5, 5, 3, 5, -1, -1, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {5, 5, 0, 5, 5, 5, 5, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {5, 5, 0, 5, 5, 5, 5, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {3, 5, 0, 5, 5, 5, 3, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {4, 4, 0, 5, 5, 5, 4, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {0, 5, 0, 5, 5, 5, 5, 5, -1, 5, -1, 5, -1, -1, -1, -1, -1, -1, -1, 5},
            {4, 4, 0, 5, 5, 5, 4, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {0, 5, 0, 5, 5, 5, 5, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {3, 0, 0, 5, 5, 0, 3, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {5, 3, 0, 5, 5, 5, 5, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
            {0, 5, 0, 5, 5, 5, 5, 5, -1, 5, -1, 5, -1, -1, 5, -1, -1, -1, -1, 5},
    };

    public static int getCollisionType(NaviSceneId curCard, NaviSceneId newCard) {
        int curIndex = curCard.ordinal();//列
        int newIndex = newCard.ordinal();//行
        Logger.i("NaviSceneRule", "行 curCard：" + curCard + ",curIndex：" + curIndex + "\n" + "列 newCard：" + newCard + ",newIndex：" + newIndex);
        int maxIndex = NaviSceneId.values().length - 1;
        Logger.i("NaviSceneRule", "maxIndex：" + maxIndex + ",COLLISION_MATRIX.length：" + COLLISION_MATRIX.length);
        //非法值;
        if (curIndex > maxIndex || newIndex > maxIndex ||
                curIndex >= COLLISION_MATRIX.length || newIndex >= COLLISION_MATRIX.length) {
            Logger.e(TAG, "SceneInvalid");
            return SceneInvalid;
        }
        return COLLISION_MATRIX[newIndex][curIndex];
    }
}
