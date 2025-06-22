package com.sgm.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RouteSecondaryPoiItemBinding;
import com.sgm.navi.service.define.search.ChildInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.view.SkinConstraintLayout;
import com.sgm.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;

public class RouteSecondaryPoiAdapter extends RecyclerView.Adapter<RouteSecondaryPoiAdapter.Holder>{
    private PoiInfoEntity mPoiInfoEntity;
    private List<ChildInfo> mChildInfoList;
    private OnItemClickListener mItemClickListener;
    private int mSelected = -1;

    public RouteSecondaryPoiAdapter() {
        mChildInfoList = new ArrayList<>();
    }

    /***
     * 设置子节点
     * @param childInfoList 子节点列表
     * @param poiInfoEntity 当前点参数
     */
    public void setChildInfoList(final List<ChildInfo> childInfoList,final PoiInfoEntity poiInfoEntity) {
        if (null == childInfoList) {
            return;
        }
        mChildInfoList.clear();
        mChildInfoList.addAll(childInfoList);
        mPoiInfoEntity = poiInfoEntity;
        notifyDataSetChanged();
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteSecondaryPoiItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_secondary_poi_item, parent, false);
        return new RouteSecondaryPoiAdapter.Holder(routeItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        if (mSelected == position) {
            holder.mTextView.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_secondary_poi_selected));
            holder.mTextView.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.bg_route_button_select_color));
            holder.mTextView.startTextViewOneTimeMarquee();
        } else {
            holder.mTextView.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_secondary_poi_unselected));
            holder.mTextView.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_defult));
        }
        holder.mTextView.setText(mChildInfoList.get(position).getShortName());
        holder.mLayout.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                final int lastSelect = mSelected;
                if (mSelected == position) {
                    mSelected = -1;
                    notifyItemChanged(lastSelect);
                    if (mItemClickListener == null) {
                        return;
                    }
                    mItemClickListener.onCancelSelectClick(mPoiInfoEntity);
                    return;
                }
                mSelected = position;
                notifyItemChanged(lastSelect);
                notifyItemChanged(mSelected);
                if (mItemClickListener == null) {
                    return;
                }
                mItemClickListener.onItemClick(mChildInfoList.get(position));
            }
        });
    }

    @Override
    public int getItemCount() {
        return mChildInfoList.size();
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteSecondaryPoiItemBinding mRouteSecondaryPoiItemBinding;
        private SkinTextView mTextView;
        private SkinConstraintLayout mLayout;

        public Holder(@NonNull final RouteSecondaryPoiItemBinding routeSecondaryPoiItemBinding) {
            super(routeSecondaryPoiItemBinding.getRoot());
            this.mRouteSecondaryPoiItemBinding = routeSecondaryPoiItemBinding;
            mRouteSecondaryPoiItemBinding.setHolder(this);
            mTextView = mRouteSecondaryPoiItemBinding.tvRouteSecondaryPoi;
            mLayout = mRouteSecondaryPoiItemBinding.lyRouteSecondaryPoi;
        }
    }

    public interface OnItemClickListener {
        /***
         * 子节点点击事件
         * @param childInfo 子节点对象
         */
        void onItemClick(ChildInfo childInfo);

        /***
         * 子节点取消选中
         * @param poiInfoEntity 父节点对象
         */
        void onCancelSelectClick(PoiInfoEntity poiInfoEntity);
    }
}
