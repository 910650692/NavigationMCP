package com.fy.navi.hmi.mapdata.adapter;

import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.util.SparseArray;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.ColorInt;
import androidx.annotation.DrawableRes;
import androidx.annotation.IdRes;
import androidx.annotation.StringRes;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.ui.view.CustomSwipeMenuLayout;

public class BaseRecyclerHolder extends RecyclerView.ViewHolder {
    private SparseArray<View> mViews;
    private View mConvertView;

    /**
     * Constructor.
     */
    public BaseRecyclerHolder(final View view) {
        super(view);
        mViews = new SparseArray<>();
        mConvertView = view;
    }

    /**
     * @deprecated
     * Use itemView instead.
     * @return 返回view
     */
    @Deprecated
    public View getConvertView() {
        return mConvertView;
    }

    /**
     * Will set the enabled of a SwipeMenuLayout.
     * @param viewId
     * @param isExpand
     * @return 设置菜单侧滑功能是否可用
     */
    public BaseRecyclerHolder setSwipeEnabled(@IdRes final int viewId, final boolean isExpand) {
        final CustomSwipeMenuLayout view = getView(viewId);
        view.setSwipeEnabled(isExpand);
        return this;
    }

    /**
     * Will set the text of a TextView.
     * @param viewId
     * @param value
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setText(@IdRes final int viewId, final CharSequence value) {
        final TextView view = getView(viewId);
        view.setText(value);
        return this;
    }

    /**
     * Will set the text of a TextView.
     * @param viewId
     * @param strId
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setText(@IdRes final int viewId, @StringRes final int strId) {
        final TextView view = getView(viewId);
        view.setText(strId);
        return this;
    }

    /**
     * Will get the text of a TextView.
     * @param viewId
     * @return 返回对应的视图
     */
    public String getText(@IdRes final int viewId) {
        final TextView view = getView(viewId);
        return view.getText().toString();
    }

    /**
     *  Will set the image of an ImageView from a resource id.
     * @param viewId
     * @param imageResId
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setImageResource(@IdRes final int viewId, @DrawableRes final int imageResId) {
        final ImageView view = getView(viewId);
        view.setImageResource(imageResId);
        return this;
    }

    /**
     * Will set background color of a view.
     * @param viewId
     * @param color
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setBackgroundColor(@IdRes final int viewId, @ColorInt final int color) {
        final View view = getView(viewId);
        view.setBackgroundColor(color);
        return this;
    }

    /**
     * Will set background of a view.
     * @param viewId
     * @param backgroundRes
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setBackgroundRes(@IdRes final int viewId, @DrawableRes final int backgroundRes) {
        final View view = getView(viewId);
        view.setBackgroundResource(backgroundRes);
        return this;
    }

    /**
     * * Will set background of a view.
     * @param viewId
     * @param backgroundRes
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setBackgroundRes(@IdRes final int viewId, final Drawable backgroundRes) {
        final View view = getView(viewId);
        view.setBackground(backgroundRes);
        return this;
    }

    /**
     * Will set text color of a TextView.
     * @param viewId
     * @param textColor
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setTextColor(@IdRes final int viewId, @ColorInt final int textColor) {
        final TextView view = getView(viewId);
        view.setTextColor(textColor);
        return this;
    }

    /**
     * Will set the image of an ImageView from a drawable.
     * @param viewId
     * @param drawable
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setImageDrawable(@IdRes final int viewId, final Drawable drawable) {
        final ImageView view = getView(viewId);
        view.setImageDrawable(drawable);
        return this;
    }

    /**
     * Add an action to set the image of an image view. Can be called multiple times.
     * @param viewId
     * @param bitmap
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setImageBitmap(@IdRes final int viewId, final Bitmap bitmap) {
        final ImageView view = getView(viewId);
        view.setImageBitmap(bitmap);
        return this;
    }

    /**
     * Set a view visibility to VISIBLE (true) or GONE (false).
     * @param viewId
     * @param visible
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setGone(@IdRes final int viewId, final boolean visible) {
        final View view = getView(viewId);
        view.setVisibility(visible ? View.VISIBLE : View.GONE);
        return this;
    }

    /**
     * Set a view visibility to VISIBLE (true) or INVISIBLE (false).
     * @param viewId
     * @param visible
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setVisible(@IdRes final int viewId, final boolean visible) {
        final View view = getView(viewId);
        view.setVisibility(visible ? View.VISIBLE : View.GONE); // INVISIBLE
        return this;
    }

    /**
     * @deprecated
     *
     * Sets the on click listener of the view.
     * @param viewId
     * @param listener
     * @return 返回对应的视图
     */
    @Deprecated
    public BaseRecyclerHolder setOnClickListener(@IdRes final int viewId, final View.OnClickListener listener) {
        final View view = getView(viewId);
        view.setOnClickListener(listener);
        return this;
    }

    /**
     * @deprecated
     *
     * Sets the listview  item click listener of the view.
     * @param viewId
     * @param listener
     * @return 返回对应的视图
     */
    @Deprecated
    public BaseRecyclerHolder setOnItemClickListener(@IdRes final int viewId, final AdapterView.OnItemClickListener listener) {
        final AdapterView view = getView(viewId);
        view.setOnItemClickListener(listener);
        return this;
    }

    /**
     * Sets the tag of the view.
     * @param viewId
     * @param tag
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setTag(@IdRes final int viewId, final Object tag) {
        final  View view = getView(viewId);
        view.setTag(tag);
        return this;
    }

    /**
     * Sets the tag of the view.
     * @param viewId
     * @param key
     * @param tag
     * @return 返回对应的视图
     */
    public BaseRecyclerHolder setTag(@IdRes final int viewId, final int key, final Object tag) {
        final View view = getView(viewId);
        view.setTag(key, tag);
        return this;
    }

    /**
     * Get the tag of the view.
     * @param viewId
     * @param <T>
     * @return 返回view
     */
    public <T extends View> T getView(@IdRes final int viewId) {
        View view = mViews.get(viewId);
        if (view == null) {
            view = itemView.findViewById(viewId);
            mViews.put(viewId, view);
        }
        return (T) view;
    }
}
