package com.sgm.navi.scene.ui.map.component;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.util.AttributeSet;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.service.define.route.RouteLightBarItem;
import com.sgm.navi.ui.view.SkinView;

import java.util.List;

public class MultiStageProgressBar extends SkinView {
    private List<RouteLightBarItem> mItems;
    private Paint mPaint;

    public MultiStageProgressBar(Context context) {
        super(context);
    }
    public MultiStageProgressBar(Context context, AttributeSet attrs) {
        super(context, attrs);
    }
    public MultiStageProgressBar(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        int width = getWidth();
        int height = getHeight();
        if (ConvertUtils.isEmpty(mItems)) {
            canvas.drawRect(0, 0, width, height,
                    getPaintInColor(ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_000000_20)));
            return;
        }
        // 绘制背景色块
        float start = 0.0f;
        for (int i = 0; i < mItems.size(); i++) {
            float itemWidth = width * mItems.get(i).getMPercent() / 100.0f;
            canvas.drawRect(start, 0, start + itemWidth, height,
                    getPaintInColor(getColor(mItems.get(i).getMStatus())));
            start += itemWidth;
        }
    }
    public void refreshTMC(List<RouteLightBarItem> routeLightBarItems) {
        mItems = routeLightBarItems;
        invalidate();
    }

    /**
     * 根据路况取对应的颜色值
     *
     * @param status 路况状态
     * @return 颜色值
     */
    private int getColor(final int status) {
        switch (status) {
            // 畅通：绿色
            case 1:
                return ResourceUtils.Companion.getInstance().getColor(R.color.auto_color_ca6d);
            // 缓行：黄色
            case 2:
                return ResourceUtils.Companion.getInstance().getColor(R.color.auto_color_f4cf4b);
            // 拥堵：红色
            case 3:
                return ResourceUtils.Companion.getInstance().getColor(R.color.auto_color_e85466);
            // 严重拥堵：深红
            case 4:
                return ResourceUtils.Companion.getInstance().getColor(R.color.auto_color_c04361);
            // 极度畅通：深绿色
            case 5:
                return ResourceUtils.Companion.getInstance().getColor(R.color.auto_color_379e4f);
            default:
                return ResourceUtils.Companion.getInstance().getColor(R.color.auto_color_ca6d);
        }
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
}
