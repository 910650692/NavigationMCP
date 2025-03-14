package com.fy.navi.ui.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.util.AttributeSet;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatTextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.ui.R;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SkinTextView extends AppCompatTextView {
    private final Context mContext;
    private int defaultTextColor = 0;
    private int selectTextColor = 0;
    private Drawable defaultBgColor;
    private Drawable selectBgColor;
    /* 是否开启水波动效 */
    private boolean isForeground = false;
    private boolean isStartMarquee = false;

    public SkinTextView(Context context) {
        this(context, null);
    }

    public SkinTextView(Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SkinTextView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.mContext = context;
        loadTextAttrs(context, attrs);
    }

    private void loadTextAttrs(Context context, @Nullable AttributeSet attrs) {
        TypedArray typedArray = context.obtainStyledAttributes(attrs, R.styleable.SkinTextView);
        selectTextColor = typedArray.getColor(R.styleable.SkinTextView_select_text_color, getCurrentTextColor());
        selectBgColor = typedArray.getDrawable(R.styleable.SkinTextView_select_background_color);
        isForeground = typedArray.getBoolean(R.styleable.SkinTextView_enable_foreground, false);
        isStartMarquee = typedArray.getBoolean(R.styleable.SkinTextView_marquee, false);
        defaultTextColor = getCurrentTextColor();
        defaultBgColor = getBackground();
        typedArray.recycle();
        initView();
    }

    @SuppressLint({"UseCompatLoadingForDrawables", "ResourceType"})
    private void initView() {
        TypedArray typedArray = mContext.getTheme().obtainStyledAttributes(new int[]{android.R.attr.selectableItemBackground});
        int attr = typedArray.getResourceId(0, 0);
        typedArray.recycle();
        if (isForeground) setForeground(mContext.getDrawable(attr));
        if (isStartMarquee) startTextViewMarquee();
    }

    @Override
    public void setSelected(boolean selected) {
        super.setSelected(selected);
        if (selected) {
            if (selectTextColor != defaultTextColor) setTextColor(selectTextColor);
            if (!ConvertUtils.isEmpty(selectBgColor)) setBackground(selectBgColor);
        } else {
            setTextColor(defaultTextColor);
            setBackground(defaultBgColor);
        }
    }

    public void startTextViewMarquee() {
        Logger.d("lvww", "开启跑马灯效果");
        setSelected(true);
        setSingleLine();
        setHorizontallyScrolling(true);
        setEllipsize(TextUtils.TruncateAt.MARQUEE);
        setMarqueeRepeatLimit(-1);
        setFocusable(true);
        setFocusableInTouchMode(true);
        requestFocus();
    }
}