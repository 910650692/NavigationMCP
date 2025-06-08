package com.fy.navi.scene.ui.navi.component;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviTmcViewBinding;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.ui.view.SkinConstraintLayout;

import java.util.List;

//路况条组件
public class ComponentTMCRes extends SkinConstraintLayout {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private Context mContext;

    public ComponentTMCRes(@NonNull final Context context) {
        super(context);
    }

    public ComponentTMCRes(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public ComponentTMCRes(@NonNull final Context context, @Nullable final AttributeSet attrs,
                           final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(context).inflate(R.layout.component_navi_tmc_res, this);
    }

    // Tag
    private List<NaviTmcInfo.NaviTmcInfoData> mTmcAreaItemsNew;
    //    private long mRestDistance;
    private boolean mIsNightMode;
    /**
     * 绘制TmcBar的公用画笔
     */
    private Paint mPaint;
    /**
     * < tmcBar累积的总距离，注意与当前路线长度不同，重算后_totalDistance不是新路线的长度，
     * 而是_distanceHasPassed加上新路线长度，避免重算后tmcBar回到起点
     */
    private long mTotalDistance;
    /**
     * tmcBar累积的已走距离,不会清零
     */
    private long mDistanceHasPassed;

    private boolean mIsHorizontal = true;
    private boolean mOffline;
    private SceneNaviTmcViewBinding mNaviTmcBinding;

    /**
     * 初始化
     *
     * @param offline        是否离线
     * @param naviTmcBinding naviTmcBinding
     */
    public void initTmcContainer(final boolean offline,
                                 final SceneNaviTmcViewBinding naviTmcBinding) {
        mNaviTmcBinding = naviTmcBinding;
        setOffline(offline);
    }

    /**
     * 更新光柱图数据view
     *
     * @param tbitem            tbitem
     * @param distanceHasPassed distanceHasPassed
     * @param totalDistance     totalDistance
     */
    public void updateTmcAreaNew(final List<NaviTmcInfo.NaviTmcInfoData> tbitem,
                                 final long distanceHasPassed,
                                 final long totalDistance) {
        setTmcAreaDataNew(tbitem, distanceHasPassed, totalDistance);
        refresh();
    }

    /**
     * @param items             items
     * @param distanceHasPassed distanceHasPassed
     * @param totalDistance     totalDistance
     */
    public void setTmcAreaDataNew(final List<NaviTmcInfo.NaviTmcInfoData> items,
                                  final long distanceHasPassed, final long totalDistance) {
        mTmcAreaItemsNew = items;
        mDistanceHasPassed = distanceHasPassed;
        mTotalDistance = totalDistance;
    }

    @Override
    protected void dispatchDraw(final Canvas canvas) {
        super.dispatchDraw(canvas);
//        Logger.d(TAG, "dispatchDraw");
        drawTmcArea(canvas);
    }

    /**
     * 绘制光柱图
     *
     * @param canvas
     */
    private void drawTmcArea(final Canvas canvas) {
//        int routeTotalLength = 0;
        final int width = getWidth();
        int height = getHeight();
        if (mIsHorizontal) {
            // TODO
        } else {
            if (mNaviTmcBinding != null) {
                height -= mNaviTmcBinding.sivCar.getHeight() / 5;
            }
        }
//        Logger.d(TAG, "paintTmcBar left:" + this.getLeft() + " top:" + this.getTop()
//                + " right:" + this.getRight() + " bottom:" + + this.getBottom()
//                + " width:" + width + " height:" + height);

        if (mTmcAreaItemsNew == null) {
            // 默认颜色
            canvas.drawRect(0, 0, width, height, getPaintInColor(getColor(NaviConstant.TMCTrafficStatus.TRAFFIC_STATUS_OPEN)));
            return;
        }
        //计算矩形内的填充高度
        final int tmcBarLength = mTmcAreaItemsNew.size();
        //计算过程中实际距离转换成像素的长度累加
        int pixelDistanceSum = 0;
        //计算过程中实际距离的累加
        int realDistanceSum = 0;
        //标记是否有需要显示Pop信息的Item
//        final boolean shouldPopItem = false;
        //距离和View高度的比率,用于在view高度和实际距离之间进行转换,单位:像素/米
//            float rateDistanceToViewHeight = (height*1.0f)/(mRouteTotalLength*1.0f);
        long hasPassedDistance = 0;
        for (NaviTmcInfo.NaviTmcInfoData item : mTmcAreaItemsNew) {
//                routeTotalLength += item.distance;
            // 已走过的路程
            if (item.getStatus() == 10) {
                hasPassedDistance = item.getDistance();
            }
        }
        // 距离和View高度的比率,用于在view高度和实际距离之间进行转换,单位:像素/米
        final float rateDistanceToView;
        if (mIsHorizontal) {
            rateDistanceToView = (width * 1.0f) / (mTotalDistance * 1.0f);
        } else {
            rateDistanceToView = (height * 1.0f) / (mTotalDistance * 1.0f);
        }
        // 车标位置
        final int carPosition;
        if (mIsHorizontal) {
            carPosition = Math.round((hasPassedDistance + mDistanceHasPassed) * rateDistanceToView);
        } else {
            carPosition = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed) * rateDistanceToView);
        }
//            UILog.d(TAG,
//                "drawTmcArea mTotalDistance:" + mTotalDistance + " hasPassedDistance:" + hasPassedDistance + " mDistanceHasPassed :" +
//                    mDistanceHasPassed);
//            UILog.d(TAG,
//                "drawTmcArea width:" + width + " rateDistanceToView:" + rateDistanceToView + " carPosition :" + carPosition);
        if (mIsHorizontal) {
            //从左往右绘制
            pixelDistanceSum = (int) (mDistanceHasPassed * rateDistanceToView);
            for (int i = 0; i < tmcBarLength; i++) {
                final NaviTmcInfo.NaviTmcInfoData item = mTmcAreaItemsNew.get(i);
                realDistanceSum += item.getDistance();
                //计算tmcBar在绘制过程中每一小段的长度
                final float itemWidth = Math.round(item.getDistance() * rateDistanceToView);
                pixelDistanceSum += itemWidth;

//                    UILog.d(TAG, "i:" + i + "--pixelDistanceSum:" + pixelDistanceSum + "--itemWidth:" + itemWidth + "--status:" + item.status);
                if (pixelDistanceSum - itemWidth >= carPosition) {
                    canvas.drawRect(pixelDistanceSum - itemWidth, 0, pixelDistanceSum, height,
                            getPaintInColor(getColor(item.getStatus())));
                } else if (pixelDistanceSum - itemWidth < carPosition && carPosition < pixelDistanceSum) {
                    canvas.drawRect(carPosition, 0, pixelDistanceSum, height,
                            getPaintInColor(getColor(item.getStatus())));
                }
            }
//                UILog.d(TAG, "--realDistanceSum:" + realDistanceSum);

        } else {
            //从上往下绘制
            for (int i = tmcBarLength - 1; i >= 0; i--) {
                final NaviTmcInfo.NaviTmcInfoData item = mTmcAreaItemsNew.get(i);
                realDistanceSum += item.getDistance();
                //计算tmcBar在绘制过程中每一小段的长度
                final float itemHeight = Math.round(item.getDistance() * rateDistanceToView);
                pixelDistanceSum += itemHeight;

                //小于当前位置的，使用交通状况对应的颜色值进行绘制;
                if (pixelDistanceSum < carPosition) {
//                    Logger.d(TAG, "1 drawRect left ："  + " top:" + (pixelDistanceSum - itemHeight)
//                            + " width:" + width + " bottom:" + pixelDistanceSum);
                    canvas.drawRect(0, pixelDistanceSum - itemHeight, width, pixelDistanceSum,
                            // 画矩形
                            getPaintInColor(getColor(item.getStatus())));
                } else if ((pixelDistanceSum - itemHeight) < carPosition) {
                    // 位置和车标重叠
//                    Logger.d(TAG, "2 drawRect left"  + " top:" + (pixelDistanceSum - itemHeight)
//                            + " width:" + width + " bottom:" + carPosition);
                    canvas.drawRect(0, pixelDistanceSum - itemHeight, width, carPosition,
                            // 画矩形
                            getPaintInColor(getColor(item.getStatus())));
                }
            }
        }


        // 走过的路使用灰色
        if (mIsHorizontal) {
            canvas.drawRect(0, 0, carPosition, height,
                    getPaintInColor(getDefaultColor(R.color.navi_color_313336_94)));
            if (pixelDistanceSum < width) {
                canvas.drawRect(pixelDistanceSum, 0, width, height,
                        getPaintInColor(getColor(-1)));
            }
        } else {
            if (height > carPosition) {
//                Logger.d(TAG, "3 drawRect left:"  + " top:" + carPosition + " width:" + width + " bottom:" + height);
//                canvas.drawRect(0, carPosition, width, height, getPaintInColor(getColor(TrafficStatus.AUTO_UNKNOWN_ERROR)));// 画矩形
            }
        }

//            setClipToOutline(true);
//            setOutlineProvider(new ViewOutlineProvider() {
//                @Override
//                public void getOutline(View view, Outline outline) {
//                    int width = view.getWidth();
//                    int height = view.getHeight();
//                    outline.setRoundRect(0, 0, width, height, width / 2);
//                }
//            });
    }

    /**
     * 获取指定颜色的画笔.
     *
     * @param color 画笔颜色
     * @return 设置成指定颜色的画笔
     */
    private Paint getPaintInColor(final int color) {
        if (mPaint == null) {
            mPaint = new Paint();
            mPaint.setAntiAlias(true);
            mPaint.setStyle(Paint.Style.FILL);
        }
        mPaint.setColor(color);
        return mPaint;
    }

    /**
     * @param resID 资源id
     * @return 颜色值
     */
    private int getDefaultColor(final int resID) {
        return getContext().getColor(resID);
    }

    /**
     * 根据路况取对应的颜色值
     *
     * @param status 路况状态
     * @return 颜色值
     */
    private int getColor(final int status) {
        Logger.d(TAG, "getColor:" + " status:" + status);
        // 网络未连接，并且不是等于已经走过的路
        if (mOffline && status != NaviConstant.TMCTrafficStatus.AUTO_UNKNOWN_ERROR) {
            return getDefaultColor(R.color.auto_color_0a80fb);
        }
        switch (status) {
            // 畅通：绿色
            case NaviConstant.TMCTrafficStatus.TRAFFIC_STATUS_OPEN:
                return getDefaultColor(R.color.auto_color_ca6d);
            // 缓行：黄色
            case NaviConstant.TMCTrafficStatus.TRAFFIC_STATUS_SLOW:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_bfa92e) : getDefaultColor(R.color.auto_color_f4cf4b);
            // 拥堵：红色
            case NaviConstant.TMCTrafficStatus.TRAFFIC_STATUS_JAM:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_bf714d) : getDefaultColor(R.color.auto_color_e85466);
            // 严重拥堵：深红
            case NaviConstant.TMCTrafficStatus.TRAFFIC_STATUS_CONGESTED:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_81132b) : getDefaultColor(R.color.auto_color_c04361);
            // 极度畅通：深绿色
            case NaviConstant.TMCTrafficStatus.TRAFFIC_STATUS_EXTREMELY_OPEN:
                return getDefaultColor(R.color.auto_color_379e4f);
            default:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_ca6d) : getDefaultColor(R.color.auto_color_ca6d);
        }
    }

    /**
     * @param isNightMode 是否夜间模式
     */
    public void setNightMode(final boolean isNightMode) {
        mIsNightMode = isNightMode;
        refresh();
    }

    /**
     * 刷新
     */
    public void refresh() {
        invalidate();
    }

    /**
     * @param offline 是否离线
     */
    public void setOffline(final boolean offline) {
        mOffline = offline;
    }
}
