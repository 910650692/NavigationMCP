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

    public ComponentTMCRes(@NonNull Context context) {
        super(context);
    }

    public ComponentTMCRes(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ComponentTMCRes(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
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

    private boolean isHorizontal = true;
    private boolean mOffline;
    private SceneNaviTmcViewBinding mNaviTmcBinding;

    /**
     * 初始化
     */
    public void initTmcContainer(boolean offline, SceneNaviTmcViewBinding naviTmcBinding) {
        mNaviTmcBinding = naviTmcBinding;
        setOffline(offline);
        invalidate();
    }

    /**
     * 更新光柱图数据view
     */
    public void updateTmcAreaNew(List<NaviTmcInfo.NaviTmcInfoData> tbitem, long distanceHasPassed, long totalDistance) {
        setTmcAreaDataNew(tbitem, distanceHasPassed, totalDistance);
        invalidate();
    }

    public void setTmcAreaDataNew(List<NaviTmcInfo.NaviTmcInfoData> items, long distanceHasPassed, long totalDistance) {
        mTmcAreaItemsNew = items;
        mDistanceHasPassed = distanceHasPassed;
        mTotalDistance = totalDistance;
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        super.dispatchDraw(canvas);
//        Logger.d(TAG, "dispatchDraw");
        drawTmcArea(canvas);
    }

    /**
     * 绘制光柱图
     *
     * @param canvas
     */
    private void drawTmcArea(Canvas canvas) {
//        int routeTotalLength = 0;
        int width = getWidth();
        int height = getHeight();
        if (isHorizontal) {
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
            canvas.drawRect(0, 0, width, height, getPaintInColor(getColor(NaviConstant.TMCTrafficStatus.TrafficStatusOpen)));
            return;
        }
        //计算矩形内的填充高度
        int tmcBarLength = mTmcAreaItemsNew.size();
        //计算过程中实际距离转换成像素的长度累加
        int pixelDistanceSum = 0;
        //计算过程中实际距离的累加
        int realDistanceSum = 0;
        //标记是否有需要显示Pop信息的Item
        boolean shouldPopItem = false;
        //距离和View高度的比率,用于在view高度和实际距离之间进行转换,单位:像素/米
//            float rateDistanceToViewHeight = (height*1.0f)/(mRouteTotalLength*1.0f);
        long hasPassedDistance = 0;
        for (NaviTmcInfo.NaviTmcInfoData item : mTmcAreaItemsNew) {
//                routeTotalLength += item.distance;
            // 已走过的路程
            if (item.status == 10) {
                hasPassedDistance = item.distance;
            }
        }
        // 距离和View高度的比率,用于在view高度和实际距离之间进行转换,单位:像素/米
        float rateDistanceToView;
        if (isHorizontal) {
            rateDistanceToView = (width * 1.0f) / (mTotalDistance * 1.0f);
        } else {
            rateDistanceToView = (height * 1.0f) / (mTotalDistance * 1.0f);
        }
        // 车标位置
        int carPosition;
        if (isHorizontal) {
            carPosition = Math.round((hasPassedDistance + mDistanceHasPassed) * rateDistanceToView);
        } else {
            carPosition = Math.round((mTotalDistance - hasPassedDistance - mDistanceHasPassed) * rateDistanceToView);
        }
//            UILog.d(TAG,
//                "drawTmcArea mTotalDistance:" + mTotalDistance + " hasPassedDistance:" + hasPassedDistance + " mDistanceHasPassed :" +
//                    mDistanceHasPassed);
//            UILog.d(TAG,
//                "drawTmcArea width:" + width + " rateDistanceToView:" + rateDistanceToView + " carPosition :" + carPosition);
        if (isHorizontal) {
            //从左往右绘制
            pixelDistanceSum = (int) (mDistanceHasPassed * rateDistanceToView);
            for (int i = 0; i < tmcBarLength; i++) {
                NaviTmcInfo.NaviTmcInfoData item = mTmcAreaItemsNew.get(i);
                realDistanceSum += item.distance;
                //计算tmcBar在绘制过程中每一小段的长度
                float itemWidth = Math.round(item.distance * rateDistanceToView);
                pixelDistanceSum += itemWidth;

//                    UILog.d(TAG, "i:" + i + "--pixelDistanceSum:" + pixelDistanceSum + "--itemWidth:" + itemWidth + "--status:" + item.status);
                if (pixelDistanceSum - itemWidth >= carPosition) {
                    canvas.drawRect(pixelDistanceSum - itemWidth, 0, pixelDistanceSum, height,
                            getPaintInColor(getColor(item.status)));
                } else if (pixelDistanceSum - itemWidth < carPosition && carPosition < pixelDistanceSum) {
                    canvas.drawRect(carPosition, 0, pixelDistanceSum, height,
                            getPaintInColor(getColor(item.status)));
                }
            }
//                UILog.d(TAG, "--realDistanceSum:" + realDistanceSum);

        } else {
            //从上往下绘制
            for (int i = tmcBarLength - 1; i >= 0; i--) {
                NaviTmcInfo.NaviTmcInfoData item = mTmcAreaItemsNew.get(i);
                realDistanceSum += item.distance;
                //计算tmcBar在绘制过程中每一小段的长度
                float itemHeight = Math.round(item.distance * rateDistanceToView);
                pixelDistanceSum += itemHeight;

                //小于当前位置的，使用交通状况对应的颜色值进行绘制;
                if (pixelDistanceSum < carPosition) {
//                    Logger.d(TAG, "1 drawRect left ："  + " top:" + (pixelDistanceSum - itemHeight)
//                            + " width:" + width + " bottom:" + pixelDistanceSum);
                    canvas.drawRect(0, pixelDistanceSum - itemHeight, width, pixelDistanceSum,
                            // 画矩形
                            getPaintInColor(getColor(item.status)));
                } else if ((pixelDistanceSum - itemHeight) < carPosition) {
                    // 位置和车标重叠
//                    Logger.d(TAG, "2 drawRect left"  + " top:" + (pixelDistanceSum - itemHeight)
//                            + " width:" + width + " bottom:" + carPosition);
                    canvas.drawRect(0, pixelDistanceSum - itemHeight, width, carPosition,
                            // 画矩形
                            getPaintInColor(getColor(item.status)));
                }
            }
        }


        // 走过的路使用灰色
        if (isHorizontal) {
            canvas.drawRect(0, 0, carPosition, height,
                    getPaintInColor(getDefaultColor(R.color.tmc_drive_past_color)));
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
    private Paint getPaintInColor(int color) {
        if (mPaint == null) {
            mPaint = new Paint();
            mPaint.setAntiAlias(true);
            mPaint.setStyle(Paint.Style.FILL);
        }
        mPaint.setColor(color);
        return mPaint;
    }

    private int getDefaultColor(int resID) {
        return getContext().getColor(resID);
    }

    /**
     * 根据路况取对应的颜色值
     *
     * @param status
     * @return
     */
    private int getColor(int status) {
        Logger.d(TAG, "getColor:" + " status:" + status);
        // 网络未连接，并且不是等于已经走过的路
        if (mOffline && status != NaviConstant.TMCTrafficStatus.AUTO_UNKNOWN_ERROR) {
            return getDefaultColor(R.color.auto_color_0a80fb);
        }
        switch (status) {
            // 畅通：绿色
            case NaviConstant.TMCTrafficStatus.TrafficStatusOpen:
                return getDefaultColor(R.color.auto_color_ca6d);
            // 缓行：黄色
            case NaviConstant.TMCTrafficStatus.TrafficStatusSlow:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_bfa92e) : getDefaultColor(R.color.auto_color_f4cf4b);
            // 拥堵：红色
            case NaviConstant.TMCTrafficStatus.TrafficStatusJam:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_bf714d) : getDefaultColor(R.color.auto_color_e85466);
            // 严重拥堵：深红
            case NaviConstant.TMCTrafficStatus.TrafficStatusCongested:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_81132b) : getDefaultColor(R.color.auto_color_c04361);
            // 极度畅通：深绿色
            case NaviConstant.TMCTrafficStatus.TrafficStatusExtremelyOpen:
                return getDefaultColor(R.color.auto_color_379e4f);
            default:
                return mIsNightMode ? getDefaultColor(R.color.auto_color_ca6d) : getDefaultColor(R.color.auto_color_ca6d);
        }
    }

    public void setNightMode(boolean isNightMode) {
        mIsNightMode = isNightMode;
        refresh();
    }

    public void refresh() {
        invalidate();
    }

    public void setOffline(boolean offline) {
        mOffline = offline;
        refresh();
    }
}
