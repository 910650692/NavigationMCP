package com.fy.navi.hmi.limit;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.service.define.aos.RestrictedAreaDetail;

import java.util.ArrayList;
import java.util.List;

/**
 * @author QiuYaWei
 * @version  \$Revision.1.0\$
 * Date: 2025/2/7
 * Description: [在这里描述文件功能]
 */
public class LimitDriverAdapter extends RecyclerView.Adapter<LimitDriverAdapter.LimitDriverViewHolder> {
    private final ArrayList<RestrictedAreaDetail> mDate = new ArrayList<>();
    private final Context mContext;

    public LimitDriverAdapter(final Context context, final List<RestrictedAreaDetail> list) {
        this.mContext = context;
        this.mDate.addAll(list);
    }

    /**
     * 设置数据
     * @param list 设置参数
     */
    public void setData(final List<RestrictedAreaDetail> list) {
        this.mDate.clear();
        this.mDate.addAll(list);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitDriverViewHolder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_policy, parent, false);
        return new LimitDriverViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final @NonNull LimitDriverViewHolder holder, final int position) {
        final RestrictedAreaDetail bean = mDate.get(position);
        holder.mTvTitle.setText(String.format(mContext.getString(R.string.limit_policy_format), position + 1));
        holder.mTvState.setVisibility(bean.getMEffect() == 1 ? View.VISIBLE : View.INVISIBLE);
        holder.mTvTime.setText(bean.getMTime());
        final String descText = bean.getMSummary() + "\n" + bean.getMDesc();
        holder.mTvDesc.setText(descText.replace("<br/>", "\n"));

    }

    @Override
    public int getItemCount() {
        return mDate.size();
    }

    public static class LimitDriverViewHolder extends RecyclerView.ViewHolder {
        private final AppCompatTextView mTvTitle;
        private final AppCompatTextView mTvState;
        private final AppCompatTextView mTvTime;
        private final AppCompatTextView mTvDesc;

        public LimitDriverViewHolder(final @NonNull View itemView) {
            super(itemView);
            mTvTitle = itemView.findViewById(R.id.tv_title);
            mTvState = itemView.findViewById(R.id.tv_state);
            mTvTime = itemView.findViewById(R.id.tv_time);
            mTvDesc = itemView.findViewById(R.id.tv_desc);
        }
    }
}
